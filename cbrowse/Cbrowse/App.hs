{-# LANGUAGE ApplicativeDo, TypeApplications, TemplateHaskell, QuasiQuotes, GHC2021, ViewPatterns, OverloadedRecordDot #-}
module Cbrowse.App where

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

data App = App
    { stateCfg  :: Config
    , stateBackend :: Some Glean.Backend
    , stateRepo :: Glean.Repo
    }

data Config = Config
  { cfgService :: Glean.ThriftSource Glean.ClientConfig
  , cfgHttp :: Maybe Int
  , cfgHttpIface :: String
  , cfgRepoName :: Text
  , cfgRepoHash :: Maybe String
  , cfgRoot :: FilePath
  , cfgStyles :: [String]
  }

type Handler = HandlerFor App

haxl :: Glean.Haxl w a -> Handler a
haxl h = do
  backend <- getsYesod stateBackend
  repo <- getsYesod stateRepo
  liftIO $ Glean.runHaxl backend repo h

