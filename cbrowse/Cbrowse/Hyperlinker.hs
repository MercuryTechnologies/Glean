{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cbrowse.Hyperlinker where

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
import Glean.Util.Range (ByteRange (..), LineOffsets (..), byteOffsetToLineCol, getLineOffsets)
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
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64, Word8)
import Options.Applicative qualified as O
import System.FilePath
import UnliftIO.Async (mapConcurrently)
import Yesod.Core hiding ((.=))

import Cbrowse.App
import Cbrowse.Api.Types

data TargetLoc
  = TargetLine !Int
  | TargetByteOffset !Int

data Target = Target
  { targetKind :: !ByteString
  , targetPath :: !ByteString
  , targetLoc :: !TargetLoc
  }

data Hyperlink = Hyperlink
  { hlBegin :: !Int
  , hlEnd :: !Int
  , hlTarget :: !Target
  }
instance NFData Hyperlink where
  rnf x = x `seq` ()

targetToApi :: LineOffsets -> Target -> TargetApi
targetToApi lineOffsets Target{..} =
  let
    (line, col) = case targetLoc of
      TargetByteOffset bo -> byteOffsetToLineCol lineOffsets (fromIntegral bo)
      TargetLine l -> (fromIntegral l, 1)
   in
    TargetApi
      { path = Text.decodeUtf8 targetPath
      , line
      , col
      , name = Text.decodeUtf8 targetKind
      }

hyperlinkToApi :: LineOffsets -> LineOffsets -> Hyperlink -> HyperlinkApi
hyperlinkToApi ownLineOffsets targetLineOffsets Hyperlink{..} =
  let
    (beginLine, beginCol) = byteOffsetToLineCol ownLineOffsets (fromIntegral hlBegin)
    (endLine, endCol) = byteOffsetToLineCol ownLineOffsets (fromIntegral hlEnd)
    target = targetToApi targetLineOffsets hlTarget
   in
    HyperlinkApi{beginLine, beginCol, endLine, endCol, target}

hyperlinkFile :: Text -> Handler [HyperlinkApi]
hyperlinkFile path = do
  root <- getsYesod (.stateCfg.cfgRoot)
  ownLineOffsets <- fileLineOffsets root (Text.encodeUtf8 path)

  links <- reportTime ("queries for " ++ Text.unpack path) $ do
    r <- haxl $ do
      cxx <- cxxGetHyperlinks path
      cm <- codeMarkupHyperlinks path
      return (cxx ++ cm)
    liftIO $ evaluate (force r)

  let referencedFiles = Set.fromList ((.hlTarget.targetPath) <$> links)
  lineOffsetsByFile <-
    Map.fromList
      <$> mapConcurrently
        (\name -> (name,) <$> fileLineOffsets root name)
        (Set.toList referencedFiles)

  pure $ (fileHyperlinkToApi ownLineOffsets lineOffsetsByFile <$> links)
 where
  fileHyperlinkToApi ownLineOffsets lineOffsetsByFile link =
    hyperlinkToApi
      ownLineOffsets
      -- the map is guaranteed to contain the file by construction
      (fromJust $ Map.lookup link.hlTarget.targetPath lineOffsetsByFile)
      link
  fileLineOffsets :: (MonadIO m) => FilePath -> ByteString -> m LineOffsets
  fileLineOffsets root name =
    let name' = Text.unpack . Text.decodeUtf8 $ name
     in getLineOffsets <$> (liftIO . BS.readFile $ root </> name')

codeMarkupHyperlinks :: Text.Text -> Glean.Haxl w [Hyperlink]
codeMarkupHyperlinks path = do
  xrefs <- Glean.search_ $
    Angle.query $
      var $ \x ->
        x
          `where_` [ wild
                       .= predicate @CodeMarkup.FileEntityXRefLocations
                         ( rec $
                             field @"file" (string path) $
                               field @"xref"
                                 x
                                 end
                         )
                   ]

  hyperlinks <- forM xrefs $ \CodeMarkup.XRefLocation{..} -> do
    file <- Glean.keyOf (CodeMarkup.location_file xRefLocation_target)
    let
      (start, length) = case xRefLocation_source of
        CodeMarkup.RangeSpan_span span ->
          ( fromIntegral $ Glean.unNat $ Src.byteSpan_start span
          , fromIntegral $ Glean.unNat $ Src.byteSpan_length span
          )
        _ -> (0, 0)
    return
      Hyperlink
        { hlBegin = start
        , hlEnd = start + length
        , hlTarget =
            Target
              { targetKind =
                  Text.encodeUtf8 $
                    CodeMarkup.location_name xRefLocation_target
              , targetPath = Text.encodeUtf8 file
              , targetLoc = TargetByteOffset $
                  case CodeMarkup.location_location xRefLocation_target of
                    CodeMarkup.RangeSpan_span span ->
                      fromIntegral (Glean.unNat (Src.byteSpan_start span))
                    _ -> 0
              }
        }

  let
    -- When there are annotations covering identical spans, prefer an
    -- annotation that points to a different file.  This is mainly to
    -- support Flow, which for a non-local reference produces two
    -- Annotations, one pointing to the import declaration and another
    -- pointing to the original declaraiton.
    unoverlap :: [Hyperlink] -> [Hyperlink]
    unoverlap links = walk links
     where
      walk (a : b : xs)
        | hlBegin a == hlBegin b && hlEnd a == hlEnd b =
            walk (preferred a b : xs)
        | otherwise = a : walk (b : xs)
      walk xs = xs

      -- prefer links that point to a different file
      preferred a b
        | targetPath (hlTarget a) /= Text.encodeUtf8 path = a
        | otherwise = b

  return $ unoverlap $ sortBy (compare `on` hlBegin) hyperlinks

-- | Find all 'Hyperlink' spans for the given file
cxxGetHyperlinks :: Text.Text -> Glean.Haxl w [Hyperlink]
cxxGetHyperlinks path = do
  -- ApplicativeDo makes these parallel:

  xref_links <- do
    filexrefs <-
      Glean.search_ $
        Glean.expanding @Cxx.FileXRefMap $
          Glean.expanding @Cxx.XRefTargets $
            Angle.query $
              predicate @Cxx.FileXRefs $
                rec $
                  field @"xmap"
                    (rec $ field @"file" (string path) end)
                    end

    let
      crossref (ByteRange{byteRange_begin = b, byteRange_length = l}, tgt) =
        fmap (Hyperlink (fromIntegral b) (fromIntegral (b + l)))
          <$> xrefTarget tgt

      unoverlap :: [(ByteRange, a)] -> [(ByteRange, a)]
      unoverlap = go 0
       where
        go !_ [] = []
        go k (x@(ByteRange{byteRange_begin = b, byteRange_length = l}, _) : xs)
          | b >= k = x : go (b + l) xs
          | otherwise = go k xs

      -- A given file can have *many* cxx1.FileXRefs facts corresponding
      -- to different compilation traces, but we only want one hyperlink
      -- for each non-overlapping source range. So we want to de-duplicate
      -- the xrefs *before* we start fetching the data about what they
      -- refer to, otherwise we overfetch.
      xrefs = unoverlap $ Set.toList $ collectXRefTargets filexrefs

    mapM crossref xrefs

  pp_links <- do
    traces <-
      Glean.search_ $
        Angle.query $
          predicate @Cxx.PPTrace $
            rec $
              field @"file" (string path) end

    let crossref (Cxx.PPEvent_include_ trace) = do
          key <- Glean.getKey (Cxx.includeTrace_include_ trace)
          let !Pp1.Include_key
                { include_key_file = file
                , include_key_pathSpan = Src.ByteSpan s l
                } = key
          fmap
            ( Hyperlink
                (fromIntegral $ Glean.unNat s)
                (fromIntegral $ Glean.unNat s + Glean.unNat l)
            )
            <$> target_locH "include" file (Glean.Nat 1)
        crossref (Cxx.PPEvent_use use) = do
          key <- Glean.getKey use
          case key of
            Pp1.Use_key
              { use_key_nameSpan = Src.ByteSpan s l
              , use_key_definition = Just (Src.Loc file line _)
              } ->
                fmap
                  ( Hyperlink
                      (fromIntegral $ Glean.unNat s)
                      (fromIntegral $ Glean.unNat s + Glean.unNat l)
                  )
                  <$> target_locH "macro" file line
            _ -> return Nothing
        crossref _ = return Nothing

    fmap catMaybes $
      mapM crossref $
        concatMap Cxx.pPTrace_key_events $
          mapMaybe Cxx.pPTrace_key traces

  return $
    unoverlap $
      sortBy order $
        catMaybes xref_links ++ pp_links
 where
  order :: Hyperlink -> Hyperlink -> Ordering
  order (Hyperlink a1 b1 _) (Hyperlink a2 b2 _) =
    compare a1 a2 <> compare b1 b2

  unoverlap [] = []
  unoverlap (h : hs) = h : go (hlEnd h) hs
   where
    go !_ [] = []
    go k (h : hs)
      | hlBegin h >= k = h : go (hlEnd h) hs
      | otherwise = go k hs

  -- Thanks to the magic of Haxl, all the Glean.getKey calls below
  -- are batched into a single request to Glean, and sharing in
  -- the results is retained.
  xrefTarget :: Cxx.XRefTarget -> Glean.Haxl w (Maybe Target)
  xrefTarget x = case x of
    Cxx.XRefTarget_declaration (Cxx.Declaration_namespace_ r) -> do
      key <- Glean.getKey r
      target_range "namespace" $ Cxx.namespaceDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_namespaceAlias r) -> do
      key <- Glean.getKey r
      target_range "namespace" $ Cxx.namespaceAliasDeclaration_key_source key
    Cxx.XRefTarget_declaration Cxx.Declaration_usingDeclaration{} ->
      return Nothing
    Cxx.XRefTarget_declaration Cxx.Declaration_usingDirective{} ->
      return Nothing
    Cxx.XRefTarget_declaration (Cxx.Declaration_record_ r) -> do
      key <- Glean.getKey r
      target_range "record" $ Cxx.recordDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_enum_ r) -> do
      key <- Glean.getKey r
      target_range "enum" $ Cxx.enumDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_typeAlias r) -> do
      key <- Glean.getKey r
      let kind = case Cxx.typeAliasDeclaration_key_kind key of
            Cxx.TypeAliasKind_Typedef -> "typedef"
            Cxx.TypeAliasKind_Using -> "using"
            Cxx.TypeAliasKind__UNKNOWN{} -> ""
      target_range
        ("type alias (" <> kind <> ")")
        $ Cxx.typeAliasDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_function_ r) -> do
      key <- Glean.getKey r
      target_range "function" $
        Cxx.functionDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_variable r) -> do
      key <- Glean.getKey r
      let mkind = case Cxx.variableDeclaration_key_kind key of
            Cxx.VariableKind_global_{} -> Just "variable"
            Cxx.VariableKind_local{} -> Just "variable"
            Cxx.VariableKind_field{} -> Just "field"
            Cxx.VariableKind_ivar{} -> Just "ivar"
            Cxx.VariableKind_EMPTY -> Nothing
      case mkind of
        Nothing -> return Nothing
        Just kind ->
          target_range kind $
            Cxx.variableDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_objcContainer r) -> do
      key <- Glean.getKey r
      let mkind = case Cxx.objcContainerDeclaration_key_id key of
            Cxx.ObjcContainerId_protocol{} -> Just "objc protocol"
            Cxx.ObjcContainerId_interface_{} -> Just "objc interface"
            Cxx.ObjcContainerId_categoryInterface{} -> Just "objc category"
            Cxx.ObjcContainerId_extensionInterface{} -> Just "objc extension"
            Cxx.ObjcContainerId_implementation{} -> Just "objc implementation"
            Cxx.ObjcContainerId_categoryImplementation{} ->
              Just "objc category implementation"
            Cxx.ObjcContainerId_EMPTY -> Nothing
      case mkind of
        Nothing -> return Nothing
        Just kind ->
          target_range kind $
            Cxx.objcContainerDeclaration_key_source key
    Cxx.XRefTarget_declaration Cxx.Declaration_EMPTY ->
      return Nothing
    Cxx.XRefTarget_declaration (Cxx.Declaration_objcMethod r) -> do
      key <- Glean.getKey r
      target_range "objc method" $
        Cxx.objcMethodDeclaration_key_source key
    Cxx.XRefTarget_declaration (Cxx.Declaration_objcProperty r) -> do
      key <- Glean.getKey r
      target_range "objc property" $
        Cxx.objcPropertyDeclaration_key_source key
    Cxx.XRefTarget_enumerator r -> do
      key <- Glean.getKey r
      target_range "enumerator" $ Cxx.enumerator_key_source key
    Cxx.XRefTarget_objcSelector{} -> return Nothing
    Cxx.XRefTarget_objcSelectorSlot{} -> return Nothing
    Cxx.XRefTarget_unknown (Src.Loc file line _) ->
      target_locH "unknown" file line
    Cxx.XRefTarget_indirect r -> do
      key <- Glean.getKey r
      xrefTarget $ Cxx.xRefIndirectTarget_key_target key
    Cxx.XRefTarget_EMPTY ->
      return Nothing

  target_range kind (Src.Range file line _ _ _) =
    target_locH kind file line

  target_locH kind file line = do
    path <- Glean.getKey (file :: Src.File)
    return
      ( Just $
          Target kind (Text.encodeUtf8 path) $
            TargetLine $
              fromIntegral $
                Glean.unNat line
      )
