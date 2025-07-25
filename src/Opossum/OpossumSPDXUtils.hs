-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Opossum.OpossumSPDXUtils
  ( spdxToOpossum
  , parseSpdxToOpossum
  ) where

import Opossum.Opossum
import Opossum.OpossumUtils

import SPDX.Document

import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as UG
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.Vector as V
import qualified System.FilePath as FP
import System.FilePath
import qualified System.IO as IO
import System.IO (Handle, hClose, hPutStrLn, stdout)
import System.Random (randomIO)

spdxFileToEA :: SPDXFile -> ExternalAttribution
spdxFileToEA (SPDXFile { _SPDXFile_SPDXID = spdxid
                       , _SPDXFile_raw = raw
                       , _SPDXFile_fileName = filename
                       , _SPDXFile_fileTypes = _
                       , _SPDXFile_checksums = _
                       , _SPDXFile_LicenseConcluded = license
                       , _SPDXFile_licenseInfoInFiles = _
                       , _SPDXFile_licenseInfoFromFiles = _
                       , _SPDXFile_licenseComments = _
                       , _SPDXFile_copyrightText = copyright
                       , _SPDXFile_comment = _
                       , _SPDXFile_noticeText = notice
                       , _SPDXFile_fileContributors = _
                       , _SPDXFile_attributionTexts = attribution
                       , _SPDXFile_fileDependencies = dependencies
                       , _SPDXFile_name = name
                       }) =
  ExternalAttribution
    { _source = ExternalAttribution_Source "SPDXFile" 100
    , _attributionConfidence = 100
    , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
    , _originId = Nothing
    , _coordinates = Coordinates Nothing Nothing Nothing Nothing Nothing
    , _copyright = Just $ T.pack copyright
    , _licenseName =
        fmap (T.pack . renderSpdxLicense) (spdxMaybeToMaybe license)
    , _licenseText = Nothing -- TODO
    , _url = Nothing
    , _criticality = Nothing
    , _flags = mempty
    }

spdxPackageToEA :: SPDXPackage -> ExternalAttribution
spdxPackageToEA (SPDXPackage { _SPDXPackage_SPDXID = spdxid
                             , _SPDXPackage_raw = raw
                             , _SPDXPackage_name = name
                             , _SPDXPackage_versionInfo = version
                             , _SPDXPackage_packageFileName = _
                             , _SPDXPackage_supplier = _
                             , _SPDXPackage_originator = _
                             , _SPDXPackage_downloadLocation = _
                             , _SPDXPackage_filesAnalyzed = _
                             , _SPDXPackage_packageVerificationCode = _
                             , _SPDXPackage_checksums = _
                             , _SPDXPackage_homepage = _
                             , _SPDXPackage_sourceInfo = _
                             , _SPDXPackage_licenseConcluded = _
                             , _SPDXPackage_licenseInfoFromFiles = _
                             , _SPDXPackage_licenseDeclared = license
                             , _SPDXPackage_licenseComments = _
                             , _SPDXPackage_copyrightText = copyright
                             , _SPDXPackage_summary = _
                             , _SPDXPackage_description = _
                             , _SPDXPackage_comment = _
                             , _SPDXPackage_attributionTexts = _
                             , _SPDXPackage_hasFiles = _
                             }) =
  ExternalAttribution
    { _source = ExternalAttribution_Source "SPDXPackage" 100
    , _attributionConfidence = 100
    , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
    , _originId = Nothing
    , _coordinates =
        Coordinates
          Nothing
          Nothing
          ((Just . T.pack) name)
          (fmap T.pack version)
          Nothing
    , _copyright =
        case copyright of
          SPDXJust copyright' -> (Just . T.pack) copyright'
          _ -> Nothing
    , _licenseName =
        fmap (T.pack . renderSpdxLicense) (spdxMaybeToMaybe license)
    , _licenseText = Nothing -- TODO
    , _url = Nothing
    , _criticality = Nothing
    , _flags = justPreselectedFlags
    }

spdxFileOrPackageToEA :: Either SPDXFile SPDXPackage -> ExternalAttribution
spdxFileOrPackageToEA (Left f) = spdxFileToEA f
spdxFileOrPackageToEA (Right p) = spdxPackageToEA p

spdxFileOrPackageToResource :: Either SPDXFile SPDXPackage -> FilePath
spdxFileOrPackageToResource =
  \case
    Left (SPDXFile {_SPDXFile_fileName = fileName}) -> fileName
    Right (SPDXPackage {_SPDXPackage_packageFileName = Just pfn}) -> pfn
    Right (SPDXPackage {_SPDXPackage_SPDXID = spdxid, _SPDXPackage_name = name}) ->
      case name of
        "" -> spdxid ++ "/"
        _ -> name ++ "/"

initToOpossum ::
     UG.Gr (Either SPDXFile SPDXPackage) SPDXRelationship
  -> [SPDXFile]
  -> ([G.LNode SPDXRelationship], Either SPDXFile SPDXPackage)
  -> Opossum
initToOpossum graph files (p, s) =
  let ea = spdxFileOrPackageToEA s
      uuid = uuidFromString' ea
      nodeToResource (k, _) =
        case G.lab graph k of
          Just spdxFileOrPackage ->
            spdxFileOrPackageToResource spdxFileOrPackage
          Nothing -> "??"
      resource =
        case p of
          [] -> spdxFileOrPackageToResource s
          _ -> (joinPath . map nodeToResource) p
      hasFilesO =
        case s of
          Right (SPDXPackage {_SPDXPackage_hasFiles = Just hasFiles}) ->
            (unshiftPathToOpossum resource .
             mconcat .
             map (initToOpossum graph files . (\f -> ([], Left f))) .
             Maybe.mapMaybe
               (\spdxid -> List.find (`matchesSPDXID` spdxid) files))
              hasFiles
          _ -> mempty
   in hasFilesO <>
      (mempty
         { _resources =
             fpToResources
               (case s of
                  Left _ -> True
                  _ -> False)
               resource
         , _externalAttributions = Map.singleton uuid ea
         , _resourcesToAttributions = Map.singleton ('/' : resource) [uuid]
         , _externalAttributionSources =
             mkExternalAttributionSources (_source ea) Nothing 500
         })

showR (SPDXRelationship _ rtype right left) =
  unwords [left, "-" ++ show rtype ++ "->", right]

ppSPDX :: SPDXDocument -> String
ppSPDX spdx =
  let (graph, idsToIdxs, _) = spdxDocumentToGraph spdx
   in G.prettify $
      G.nemap
        spdxFileOrPackageToResource
        _SPDXRelationship_relationshipType
        graph

spdxToOpossum :: SPDXDocument -> Opossum
spdxToOpossum (spdx@SPDXDocument { _SPDX_SPDXID = spdxid
                                 , _SPDX_comment = _
                                 , _SPDX_creationInfo = _
                                 , _SPDX_name = name
                                 , _SPDX_files = files
                                 , _SPDX_packages = packages
                                 , _SPDX_relationships = relationships
                                 }) =
  let (graph, idsToIdxs, _) = spdxDocumentToGraph spdx
      addS ::
           [G.LNode SPDXRelationship]
        -> Maybe ([G.LNode SPDXRelationship], Either SPDXFile SPDXPackage)
      addS [] = Nothing
      addS path =
        let (node, _) = last path
         in case G.lab graph node of
              Just s -> Just (path, s)
              _ -> Nothing
      pieces =
        (map (initToOpossum graph files) .
         concatMap
           (Maybe.mapMaybe addS .
            List.nub . concatMap List.inits . map (reverse . G.unLPath)) .
         map
           (\root ->
              let indx = Map.findWithDefault undefined root idsToIdxs
               in G.lbft indx graph :: [G.LPath SPDXRelationship]) .
         getRootsFromDocument)
          spdx
   in mconcat
        (mempty
           { _metadata =
               Map.fromList
                 [ ("projectId", A.toJSON spdxid)
                 , ("projectTitle", A.toJSON name)
                 ]
           } :
         pieces)

parseSpdxToOpossum :: FilePath -> IO Opossum
parseSpdxToOpossum inputPath = do
  hPutStrLn IO.stderr ("parse: " ++ inputPath)
  spdx <- parseSPDXDocument inputPath
  let opossum = spdxToOpossum spdx
  return (normaliseOpossum opossum)
