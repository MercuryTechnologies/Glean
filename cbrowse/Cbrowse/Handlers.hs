{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cbrowse.Handlers where

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
import Data.Aeson qualified as A
import Data.Aeson.TH qualified as A
import Data.Binary.Builder (Builder)
import Data.Binary.Builder qualified as Builder
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (ord)
import Data.List (sort, sortBy)
import Data.Maybe
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import Options.Applicative qualified as O
import System.FilePath
import Yesod.Core hiding ((.=))
import Yesod.Core.Types

import Cbrowse.App
import Cbrowse.Hyperlinker

getRootR :: Handler (JSONResponse Text)
getRootR = do
  pure . JSONResponse $ "Hello"

getSourceR :: [Text] -> Handler Text
getSourceR pathPieces = do
  when (any (== "..") pathPieces) $
    invalidArgs ["path traversal"]

  root <- getsYesod (.stateCfg.cfgRoot)

  let path = foldl' (</>) root (Text.unpack <$> pathPieces)

  sendFile typePlain path

data AnnotationsResp = AnnotationsResp
  { hyperlinks :: [HyperlinkApi]
  }

$(A.deriveJSON A.defaultOptions ''AnnotationsResp)

getAnnotationsR :: [Text] -> Handler (JSONResponse AnnotationsResp)
getAnnotationsR pathPieces = do
  when (any (== "..") pathPieces) $
    invalidArgs ["path traversal"]

  let path = Text.intercalate "/" pathPieces

  hyperlinks <- hyperlinkFile path
  pure . JSONResponse $ AnnotationsResp {hyperlinks}
