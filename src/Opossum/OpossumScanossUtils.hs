-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Opossum.OpossumScanossUtils
  ( parseScanossToOpossum
  , parseScanossBS
  , ScanossFindings(..)
  , ScanossFinding(..)
  , ScanossFindingLicense(..)
  , ScanossFindingCopyright(..)
  , ScanossFindingDependency(..)
  ) where

import           Opossum.Opossum
import           Opossum.OpossumUtils

import qualified Control.Monad.State        as MTL
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.Aeson.Types           as A
import qualified Data.Aeson.KeyMap          as AKM
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List                  (intercalate)
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, maybeToList)
import           Data.Maybe                 as Maybe
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Distribution.Parsec        as SPDX
import qualified Distribution.SPDX          as SPDX
import           SPDX.LicenseExpression     as SPDX
import qualified System.FilePath            as FP
import           System.IO                  (Handle, hClose, hPutStrLn, stdout)
import qualified System.IO                  as IO
import           System.Random              (randomIO)

import           Debug.Trace                (trace)

{-
        {
          "vendor": "Ezekiel",
          "component": "rollup-plugin-uglify-es",
          "version": "0.0.1",
          "source": "component_declared"
        },
-}
data ScanossFindingDependency =
  ScanossFindingDependency
    { _ScanossFindingDependency_vendor    :: String
    , _ScanossFindingDependency_component :: String
    , _ScanossFindingDependency_version   :: String
    , _ScanossFindingDependency_source    :: String
    }

instance A.FromJSON ScanossFindingDependency where
  parseJSON =
    A.withObject "ScanossFindingDependency" $ \v -> do
      ScanossFindingDependency <$> v A..: "vendor" <*> v A..: "component" <*>
        v A..: "version" <*>
        v A..: "source"

data ScanossFindingLicense =
  ScanossFindingLicense
    { _ScanossFindingLicense        :: T.Text
    , _ScanossFindingLicense_source :: String
    }

instance A.FromJSON ScanossFindingLicense where
  parseJSON =
    A.withObject "ScanossFindingLicense" $ \v -> do
      ScanossFindingLicense <$> v A..: "name" <*> v A..: "source"

data ScanossFindingCopyright =
  ScanossFindingCopyright
    { _ScanossFindingCopyright        :: T.Text
    , _ScanossFindingCopyright_source :: String
    }

instance A.FromJSON ScanossFindingCopyright where
  parseJSON =
    A.withObject "ScanossFindingCopyright" $ \v -> do
      ScanossFindingCopyright <$> v A..: "name" <*> v A..: "source"

{-
{
  "./yarn.lock": [
    {
      "id": "snippet",
      "status": "pending",
      "lines": "13277-13789,12930-13064,12556-12851,12257-12371,11605-11973",
      "oss_lines": "15730-16242,15002-15136,14490-14785,14292-14406,13579-13947",
      "matched": "10%",
      "purl": [
        "pkg:github/leclercb/taskunifier-app"
      ],
      "vendor": "leclercb",
      "component": "taskunifier-app",
      "version": "1.2.9",
      "latest": "1.2.9",
      "url": "https://github.com/leclercb/taskunifier-app",
      "release_date": "2021-07-22",
      "file": "yarn.lock",
      "url_hash": "aee3ccf9020c05567ace82879a2ab64d",
      "file_hash": "afaf765786c8dbdd11acd7b01fb25ee3",
      "file_url": "https://osskb.org/api/file_contents/afaf765786c8dbdd11acd7b01fb25ee3",
      "dependencies": [
        {
          "vendor": "Ezekiel",
          "component": "rollup-plugin-uglify-es",
          "version": "0.0.1",
          "source": "component_declared"
        },
        {
          "vendor": "Fractal",
          "component": "gulp-util",
          "version": "^3.0.8",
          "source": "component_declared"
        },
      ],
      "licenses": [
        {
          "name": "AGPL-3.0",
          "source": "component_declared"
        }
      ],
      "copyrights": [
        {
          "name": "Copyright .. 2012...2016 Kir Belevich",
          "source": "license_file"
        },
        {
          "name": "Copyright 2012-2018 (c) Mihai Bazon <mihai.bazon@gmail.com>",
          "source": "license_file"
        },
        {
          "name": "copyright owner or entity authorized by",
          "source": "license_file"
        }
      ],
      "vulnerabilities": [],
      "quality": [],
      "cryptography": [],
      "server": {
        "hostname": "p10",
        "version": "4.2.9",
        "flags": "0",
        "elapsed": "1.110124s"
      }
    }
  ],
  "./scripts/fossid-to-opossum-converter/process_larger_fossid_json.py": [
    {
      "id": "none",
      "server": {
        "hostname": "p8",
        "version": "4.2.9",
        "flags": "0",
        "elapsed": "0.280415s"
      }
    }
  ],
  ...
-}
data ScanossFinding =
  ScanossFinding
    { _ScanossFinding_id           :: String
    , _ScanossFinding_matched      :: String
    , _ScanossFinding_purl         :: [PURL]
    , _ScanossFinding_vendor       :: Maybe T.Text
    , _ScanossFinding_component    :: Maybe T.Text
    , _ScanossFinding_version      :: Maybe T.Text
    , _ScanossFinding_latest       :: Maybe T.Text
    , _ScanossFinding_url          :: Maybe T.Text
  -- , _ScanossFinding_release_date": "2021-07-22",
  -- , _ScanossFinding_file": "yarn.lock",
  -- , _ScanossFinding_url_hash": "aee3ccf9020c05567ace82879a2ab64d",
  -- , _ScanossFinding_file_hash": "afaf765786c8dbdd11acd7b01fb25ee3",
    , _ScanossFinding_file_url     :: Maybe String
    , _ScanossFinding_dependencies :: [ScanossFindingDependency]
    , _ScanossFinding_licenses     :: [ScanossFindingLicense]
    , _ScanossFinding_copyrights   :: [ScanossFindingCopyright]
  -- , _ScanossFinding_vulnerabilities": [],
  -- , _ScanossFinding_quality": [],
  -- , _ScanossFinding_cryptography": [],
    , _ScanossFinding_comment      :: Maybe T.Text
    }

instance A.FromJSON ScanossFinding where
  parseJSON =
    A.withObject "ScanossFinding" $ \v -> do
      let keysToFilter =
            [ "server"
            , "cryptography"
            , "quality"
            , "copyrights"
            , "licenses"
            , "file_hash"
            , "url_hash"
            ]
      let comment =
            (Just .
             T.pack .
             C8.unpack .
             A.encodePretty .
             AKM.filterWithKey (\key -> const (not (key `elem` keysToFilter))))
              v
      ScanossFinding <$> v A..: "id" <*> v A..: "matched" <*>
        (fmap (Maybe.mapMaybe (parsePURL)) $ v A..: "purl") <*>
        v A..:? "vendor" <*>
        v A..:? "component" <*>
        v A..:? "version" <*>
        v A..:? "latest" <*>
        v A..:? "url" <*>
        v A..:? "file_url" <*>
        (fmap (Maybe.fromMaybe []) $ v A..:? "dependencies") <*>
        (fmap (Maybe.fromMaybe []) $ v A..:? "licenses") <*>
        (fmap (Maybe.fromMaybe []) $ v A..:? "copyrights") <*>
        return comment

data ScanossFindings =
  ScanossFindings (V.Vector ScanossFinding)

instance A.FromJSON ScanossFindings where
  parseJSON =
    A.withArray "ScanossFindings" $ \a -> do
      fs <-
        V.mapMaybeM
          (A.withObject "ScanossFinding" $ \v -> do
             id' <- v A..: "id" :: A.Parser String
             if id' == "none"
               then return Nothing
               else do
                 f <- (A.parseJSON (A.Object v)) :: A.Parser ScanossFinding
                 return $ Just f)
          a
      return $ ScanossFindings fs

scanossFindingToOpossumCoordinates :: ScanossFinding -> Coordinates
scanossFindingToOpossumCoordinates (ScanossFinding { _ScanossFinding_vendor = vendor
                                                   , _ScanossFinding_component = component
                                                   , _ScanossFinding_version = version
                                                   , _ScanossFinding_purl = purl
                                                   }) =
  case purl of
    (p:_) -> purlToCoordinates p
    _     -> Coordinates component vendor Nothing version Nothing

scanossFindingsToOpossum :: (String, ScanossFindings) -> IO Opossum
scanossFindingsToOpossum (fn, ScanossFindings fs) =
  let scanossFindingToOpossum :: ScanossFinding -> IO Opossum
      scanossFindingToOpossum f = do
        uuid <- randomIO
        let source =
              ExternalAttribution_Source ("SCANOSS-" ++ _ScanossFinding_id f) 10
        let resources = fpToResources True fn
        let copyrights =
              ((Just . T.unlines . map _ScanossFindingCopyright)
                 (_ScanossFinding_copyrights f))
        let licenses =
              ((Just .
                T.intercalate " AND " .
                map (\l -> T.unwords ["(", l, ")"]) . map _ScanossFindingLicense)
                 (_ScanossFinding_licenses f))
        let ea =
              ExternalAttribution
                source
                10
                (_ScanossFinding_comment f)
                Nothing
                (scanossFindingToOpossumCoordinates f)
                copyrights
                licenses
                Nothing
                (_ScanossFinding_url f)
                Nothing
                mempty
        let eas = mkExternalAttributionSources source Nothing 5
        -- TODO: dependencies
        return $
          mempty
            { _resources = resources
            , _externalAttributions = Map.singleton uuid ea
            , _resourcesToAttributions = (Map.singleton ("/" FP.</> fn) [uuid])
            }
   in do let o = mempty {_resources = fpToResources True fn}
         os <- V.mapM scanossFindingToOpossum fs
         (return . (o <>) . mconcat . V.toList) os

parseScanossBS :: B.ByteString -> IO Opossum
parseScanossBS bs =
  case (A.eitherDecode bs :: Either String (Map.Map String ScanossFindings)) of
    Right scanossMap ->
      mconcat $ map scanossFindingsToOpossum (Map.assocs scanossMap)
    Left err -> do
      putStrLn err
      undefined -- TODO

parseScanossToOpossum :: FilePath -> IO Opossum
parseScanossToOpossum inputPath = do
  hPutStrLn IO.stderr ("parse: " ++ inputPath)
  let baseOpossum =
        mempty
          { _metadata =
              Map.fromList
                [ ("projectId", A.toJSON ("0" :: String))
                , ("projectTitle", A.toJSON inputPath)
                , ("fileCreationDate", A.toJSON ("" :: String))
                ]
          }
  opossum <- B.readFile inputPath >>= parseScanossBS
  return (normaliseOpossum (baseOpossum <> opossum))
