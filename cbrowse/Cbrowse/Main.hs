{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cbrowse.Main where

import Control.Monad
import Data.Function
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Schema.Codemarkup.Types qualified as CodeMarkup
import Glean.Schema.CodemarkupTypes.Types qualified as CodeMarkup
import Glean.Schema.Cxx1.Types qualified as Cxx
import Glean.Schema.Pp1.Types qualified as Pp1
import Glean.Schema.Src.Types qualified as Src

import Glean qualified
import Glean.Angle as Angle
import Glean.Impl.ConfigProvider
import Glean.Remote qualified
import Glean.Util.ConfigProvider
import Glean.Util.Range (ByteRange (..), byteOffsetToLineCol, getLineOffsets)
import Glean.Util.Some
import Glean.Util.XRefs (collectXRefTargets)

import Util.EventBase (withEventBaseDataplane)
import Util.Log
import Util.OptParse
import Util.Timing

import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Control.DeepSeq
import Control.Exception hiding (Handler)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Reader qualified as Reader
import Data.Binary.Builder (Builder)
import Data.Binary.Builder qualified as Builder
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (ord)
import Data.List (sort, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import Network.Wai.Middleware.Cors
import Options.Applicative qualified as O
import System.FilePath
import Yesod.Core hiding ((.=))
import Yesod.Core.Types

import Cbrowse.App
import Cbrowse.Foundation
import Cbrowse.Hyperlinker

allowedHeaders :: [HTTP.HeaderName]
allowedHeaders = ["X-XSRF-TOKEN", "DNT", "Keep-Alive", "User-Agent", "X-Requested-With", "If-Modified-Since", "Cache-Control", "Content-Type", "Content-Range", "Range", "Authorization", "Accept", "Origin", "Bundle-Version", "Git-Commit", "EC2-Instance-Name", "X-CSRF-PROTECT", "X-Frontend-Path", "Content-Disposition"]

corsMiddleware :: Wai.Middleware
corsMiddleware =
  cors
    ( const . Just $
        CorsResourcePolicy
          { -- TODO: wrong!
            corsOrigins = Nothing
          , corsMethods = ["HEAD", "GET", "OPTIONS", "POST", "PUT", "DELETE", "CONNECT", "PATCH", "TRACE"]
          , corsRequestHeaders = allowedHeaders
          , corsExposedHeaders = Just allowedHeaders
          , corsMaxAge = Just (24 * 60 * 60) -- 24 hours is Firefox's cap. Chrome is 2 hours. https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Max-Age
          , corsVaryOrigin = True
          , corsRequireOrigin = False
          , corsIgnoreFailures = False
          }
    )

options :: O.ParserInfo Config
options = O.info (O.helper <*> parser) O.fullDesc
 where
  parser =
    Config
      <$> Glean.Remote.options
      <*> O.optional
        ( O.option
            O.auto
            (O.long "http" <> O.metavar "PORT")
        )
      <*> O.strOption (O.long "http-iface" <> O.metavar "IFACE" <> O.value "*6")
      <*> textOption (O.long "repo" <> O.metavar "NAME" <> O.value "fbsource")
      <*> O.optional (O.strOption (O.long "repo-hash" <> O.metavar "HASH"))
      <*> O.strOption (O.long "root" <> O.metavar "PATH" <> O.value "")
      <*> O.many
        ( O.option
            (O.maybeReader style)
            (O.long "highlight" <> O.short 'l' <> O.metavar "KIND:COLOUR")
        )

  style s
    | (kind, ':' : color) <- break (== ':') s =
        Just $ '.' : kind ++ " { background-color: " ++ color ++ "; }"
    | otherwise = Nothing

middleware :: [Wai.Middleware]
middleware =
  [ corsMiddleware
  ]

main :: IO ()
main = do
  withConfigOptions options $ \(cfg, cfgOpts) ->
    withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \(configAPI :: ConfigAPI) -> do
        Glean.Remote.withRemoteBackend
          evb
          configAPI
          (cfgService cfg)
          (Just schema_id)
          $ \backend -> do
            repo <- case cfgRepoHash cfg of
              Nothing -> Glean.getLatestRepo backend (cfgRepoName cfg)
              Just hash ->
                return $
                  Glean.Repo
                    (cfgRepoName cfg)
                    (fromString hash)

            let app =
                  App
                    { stateCfg = cfg
                    , stateBackend = Some backend
                    , stateRepo = repo
                    }

            whenJust (cfgHttp cfg) $ \port -> do
              Warp.runSettings
                ( Warp.setPort port $
                    Warp.setHost
                      (fromString $ cfgHttpIface cfg)
                      Warp.defaultSettings
                )
                . (foldl1 (.) middleware)
                =<< toWaiApp app
