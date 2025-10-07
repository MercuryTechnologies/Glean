{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cbrowse.Api.Types where

import Data.Aeson qualified as A
import Data.Aeson.TH qualified as A
import Data.Word (Word64, Word8)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

{- | Hyperlink for the API. Uses line/col because we can compute those quickly
server side and only make the client deal with UTF-16 nonsense within a
line.
-}
data HyperlinkApi = HyperlinkApi
  { beginLine :: !Word64
  , beginCol :: !Word64
  , endLine :: !Word64
  , endCol :: !Word64
  , target :: !TargetApi
  }
  deriving stock (Show)

data TargetApi = TargetApi
  { path :: !Text
  , line :: !Word64
  , col :: !Word64
  , name :: !Text
  }
  deriving stock (Show)

$(A.deriveJSON A.defaultOptions ''TargetApi)
$(A.deriveJSON A.defaultOptions ''HyperlinkApi)

data ReferenceApi = ReferenceApi
  { target :: !TargetApi
  }

$(A.deriveJSON A.defaultOptions ''ReferenceApi)
