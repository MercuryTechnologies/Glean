{-# LANGUAGE ApplicativeDo, TypeApplications, TemplateHaskell, QuasiQuotes, GHC2021, ViewPatterns, OverloadedRecordDot #-}
module Cbrowse.Main where

import Data.Function
import Control.Monad
import Glean.Schema.Builtin.Types (schema_id)
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Pp1.Types as Pp1
import qualified Glean.Schema.Codemarkup.Types as CodeMarkup
import qualified Glean.Schema.CodemarkupTypes.Types as CodeMarkup

import qualified Glean
import qualified Glean.Remote
import Glean.Impl.ConfigProvider
import Glean.Angle as Angle
import Glean.Util.ConfigProvider
import Glean.Util.Range (ByteRange(..), byteOffsetToLineCol, getLineOffsets)
import Glean.Util.XRefs (collectXRefTargets)
import Glean.Util.Some

import Util.EventBase (withEventBaseDataplane)
import Util.Log
import Util.OptParse
import Util.Timing

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Control.DeepSeq
import Control.Exception hiding (Handler)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import Data.Char (ord)
import Data.List (sort, sortBy)
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import qualified Options.Applicative as O
import System.FilePath
import Yesod.Core hiding ((.=))
import Yesod.Core.Types

import Cbrowse.App
import Cbrowse.Hyperlinker
import Cbrowse.Foundation

options :: O.ParserInfo Config
options = O.info (O.helper <*> parser) O.fullDesc
  where
    parser = Config
      <$> Glean.Remote.options
      <*> O.optional (O.option O.auto
            (O.long "http" <> O.metavar "PORT"))
      <*> O.strOption (O.long "http-iface" <> O.metavar "IFACE" <> O.value "*6")
      <*> textOption (O.long "repo" <> O.metavar "NAME" <> O.value "fbsource")
      <*> O.optional (O.strOption (O.long "repo-hash" <> O.metavar "HASH"))
      <*> O.strOption (O.long "root" <> O.metavar "PATH" <> O.value "")
      <*> O.many (O.option (O.maybeReader style)
            (O.long "highlight" <> O.short 'l' <> O.metavar "KIND:COLOUR"))

    style s
      | (kind,':':color) <- break (==':') s =
          Just $ '.' : kind ++ " { background-color: " ++ color ++ "; }"
      | otherwise = Nothing


main :: IO ()
main = do
  withConfigOptions options $ \(cfg, cfgOpts) ->
    withEventBaseDataplane $ \evb ->
    withConfigProvider cfgOpts $ \(configAPI :: ConfigAPI) -> do
      Glean.Remote.withRemoteBackend evb configAPI
        (cfgService cfg) (Just schema_id) $ \backend -> do
        repo <- case cfgRepoHash cfg of
          Nothing -> Glean.getLatestRepo backend (cfgRepoName cfg)
          Just hash -> return $ Glean.Repo
            (cfgRepoName cfg)
            (fromString hash)

        let app = App
              { stateCfg = cfg
              , stateBackend = Some backend
              , stateRepo = repo
              }

        whenJust (cfgHttp cfg) $ \port -> do
          Warp.runSettings
            (Warp.setPort port
              $ Warp.setHost (fromString $ cfgHttpIface cfg)
              Warp.defaultSettings)
            =<< toWaiApp app

