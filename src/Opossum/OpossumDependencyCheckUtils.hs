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

module Opossum.OpossumDependencyCheckUtils
  ( parseDependencyCheckToOpossum
  , parseDependencyCheckBS
  , DependencyCheckPackage(..)
  , DependencyCheckDependency(..)
  , DependencyCheckFile(..)
  ) where

import           Opossum.Opossum
import           Opossum.OpossumUtils

import qualified Control.Monad.State      as MTL
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types         as A
import qualified Data.ByteString.Lazy     as B
import           Data.List                (intercalate)
import qualified Data.List                as List
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, maybeToList)
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Vector              as V
import qualified Distribution.Parsec      as SPDX
import qualified Distribution.SPDX        as SPDX
import qualified System.FilePath          as FP
import           System.IO                (Handle, hClose, hPutStrLn, stdout)
import qualified System.IO                as IO
import           System.Random            (randomIO)

import           Debug.Trace              (trace)

type DependencyCheckConfidence = String

{-
            "evidenceCollected": {
                "vendorEvidence": [
                    {
                        "type": "vendor",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    }
                ],
                "productEvidence": [
                    {
                        "type": "product",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    }
                ],
                "versionEvidence": [
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    },
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "version",
                        "value": "4"
                    }
                ]
            }
 -}
data DependencyCheckEvidence =
  DependencyCheckEvidence
    { _dce_type       :: String
    , _dce_confidence :: DependencyCheckConfidence
    , _dce_source     :: String
    , _dce_name       :: String
    , _dce_value      :: String
    }
  deriving (Eq, Show)

instance A.FromJSON DependencyCheckEvidence where
  parseJSON =
    A.withObject "DependencyCheckEvidence" $ \v -> do
      DependencyCheckEvidence <$> v A..: "type" <*> v A..: "confidence" <*>
        v A..: "source" <*>
        v A..: "name" <*>
        v A..: "value"
 {-
                #foreach($id in $dependency.getSoftwareIdentifiers())
                    #if($foreach.count > 1),#end
                    {
                        "id": "$enc.json($id.value)"
                        #if($id.confidence),"confidence": "$enc.json($id.confidence)"#end
                        #if($id.url),"url": "$enc.json($id.url)"#end
                        #if($id.description),"description": "$enc.json($id.description)"#end
                        #if($id.notes),"notes": "$enc.json($id.notes)"#end
                    }
                #end
  -}

{-
            "packages": [
                {
                    "id": "pkg:nuget\/Antlr4BuildTasks@8.13",
                    "confidence": "HIGHEST",
                    "url": "https:\/\/ossindex.sonatype.org\/component\/pkg:nuget\/Antlr4BuildTasks@8.13?utm_source=dependency-check&utm_medium=integration&utm_content=6.2.2"
                }
            ],
 -}
data DependencyCheckPackage =
  DependencyCheckPackage
    { _dcp_id          :: Either String Purl
    , _dcp_url         :: Maybe T.Text
    , _dcp_description :: Maybe String
    , _dcp_confidence  :: Maybe DependencyCheckConfidence
  -- , _dcp_notes :: Maybe String
    }
  deriving (Eq, Show)

instance A.FromJSON DependencyCheckPackage where
  parseJSON =
    A.withObject "DependencyCheckPackage" $ \v -> do
      DependencyCheckPackage <$>
        fmap
          (\raw ->
             (\case
                Just purl -> Right purl
                Nothing   -> Left raw)
               (parsePurl raw))
          (v A..: "id") <*>
        v A..:? "url" <*>
        v A..:? "description" <*>
        v A..:? "confidence"

{-
see also: https://github.com/jeremylong/DependencyCheck/blob/main/core/src/main/resources/templates/jsonReport.vsl
-}
{-
{
    "reportSchema": "1.1",
    "scanInfo": {
        "engineVersion": "6.2.2",
        "dataSource": [
            {
                "name": "NVD CVE Checked",
                "timestamp": "2021-09-18T09:12:35"
            },
            {
                "name": "NVD CVE Modified",
                "timestamp": "2021-09-18T05:00:01"
            },
            {
                "name": "VersionCheckOn",
                "timestamp": "2021-09-18T09:12:35"
            }
        ]
    },
    "projectInfo": {
        "name": "",
        "reportDate": "2021-09-18T09:17:44.104464Z",
        "credits": {
            "NVD": "This report contains data retrieved from the National Vulnerability Database: http://nvd.nist.gov",
            "NPM": "This report may contain data retrieved from the NPM Public Advisories: https://www.npmjs.com/advisories",
            "RETIREJS": "This report may contain data retrieved from the RetireJS community: https://retirejs.github.io/retire.js/",
            "OSSINDEX": "This report may contain data retrieved from the Sonatype OSS Index: https://ossindex.sonatype.org"
        }
    },
    "dependencies": [
        {
            "isVirtual": false,
            "fileName": "Antlr4.Test.csproj",
            "filePath": "\/output\/input\/runtime-testsuite\/test\/org\/antlr\/v4\/test\/runtime\/csharp\/Antlr4.Test.csproj",
            "md5": "84395c10d62f395d2b40ed52f6ea434a",
            "sha1": "ad06e20a662f6f4eb73ddd3bb24efe3bb5dfd12d",
            "sha256": "6795e7470f4f23990125b93338f210462e8e5444088b66d3c3001337e705d6f1",
            "evidenceCollected": {
                "vendorEvidence": [
                    {
                        "type": "vendor",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    }
                ],
                "productEvidence": [
                    {
                        "type": "product",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    }
                ],
                "versionEvidence": [
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4.Test"
                    },
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "version",
                        "value": "4"
                    }
                ]
            }
        },
        {
            "isVirtual": false,
            "fileName": "Antlr4.csproj",
            "filePath": "\/output\/input\/runtime\/CSharp\/src\/Antlr4.csproj",
            "md5": "ea6f530c6c4136e69b799e5c7b811fe5",
            "sha1": "f5da7cc9b57dacd763f7a475baf78490d0c94b8c",
            "sha256": "d71336ff88c53adb244872b7124274137fd9e0566786dbde6b2ae5a12ecca2e0",
            "evidenceCollected": {
                "vendorEvidence": [
                    {
                        "type": "vendor",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4"
                    }
                ],
                "productEvidence": [
                    {
                        "type": "product",
                        "confidence": "HIGH",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4"
                    }
                ],
                "versionEvidence": [
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "name",
                        "value": "Antlr4"
                    },
                    {
                        "type": "version",
                        "confidence": "MEDIUM",
                        "source": "file",
                        "name": "version",
                        "value": "4"
                    }
                ]
            }
        },
        {
            "isVirtual": true,
            "fileName": "Antlr4BuildTasks:8.13",
            "filePath": "\/output\/input\/runtime\/CSharp\/tests\/issue-2693\/Test.csproj",
            "evidenceCollected": {
                "vendorEvidence": [
                    {
                        "type": "vendor",
                        "confidence": "LOW",
                        "source": "msbuild",
                        "name": "id",
                        "value": "Antlr4BuildTasks"
                    }
                ],
                "productEvidence": [
                    {
                        "type": "product",
                        "confidence": "HIGHEST",
                        "source": "msbuild",
                        "name": "id",
                        "value": "Antlr4BuildTasks"
                    }
                ],
                "versionEvidence": [
                    {
                        "type": "version",
                        "confidence": "HIGHEST",
                        "source": "msbuild",
                        "name": "version",
                        "value": "8.13"
                    }
                ]
            },
            "packages": [
                {
                    "id": "pkg:nuget\/Antlr4BuildTasks@8.13",
                    "confidence": "HIGHEST",
                    "url": "https:\/\/ossindex.sonatype.org\/component\/pkg:nuget\/Antlr4BuildTasks@8.13?utm_source=dependency-check&utm_medium=integration&utm_content=6.2.2"
                }
            ],
            "vulnerabilityIds": [
                {
                    "id": "cpe:2.3:a:tasks:tasks:8.13:*:*:*:*:*:*:*",
                    "confidence": "LOW"
                }
            ],
            "vulnerabilities": [
                {
                    "source": "NVD",
                    "name": "CVE-2020-22475",
                    "severity": "MEDIUM",
                    "cvssv2": {
                        "score": 4.6,
                        "accessVector": "LOCAL",
                        "accessComplexity": "LOW",
                        "authenticationr": "NONE",
                        "confidentialImpact": "PARTIAL",
                        "integrityImpact": "PARTIAL",
                        "availabilityImpact": "PARTIAL",
                        "severity": "MEDIUM",
                        "version": "2.0",
                        "exploitabilityScore": "3.9",
                        "impactScore": "6.4"
                    },
                    "cvssv3": {
                        "baseScore": 6.8,
                        "attackVector": "PHYSICAL",
                        "attackComplexity": "LOW",
                        "privilegesRequired": "NONE",
                        "userInteraction": "NONE",
                        "scope": "UNCHANGED",
                        "confidentialityImpact": "HIGH",
                        "integrityImpact": "HIGH",
                        "availabilityImpact": "HIGH",
                        "baseSeverity": "MEDIUM",
                        "exploitabilityScore": "0.9",
                        "impactScore": "5.9",
                        "version": "3.1"
                    },
                    "cwes": [
                        "CWE-276"
                    ],
                    "description": "\"Tasks\" application version before 9.7.3 is affected by insecure permissions. The VoiceCommandActivity application component allows arbitrary applications on a device to add tasks with no restrictions.",
                    "notes": "",
                    "references": [
                        {
                            "source": "MISC",
                            "url": "https:\/\/lyhinslab.org\/index.php\/2020\/07\/18\/how-the-white-box-hacking-works-ok-google-i-wanna-pwn-this-app\/",
                            "name": "https:\/\/lyhinslab.org\/index.php\/2020\/07\/18\/how-the-white-box-hacking-works-ok-google-i-wanna-pwn-this-app\/"
                        },
                        {
                            "source": "MISC",
                            "url": "https:\/\/www.exploit-db.com\/exploits\/49563",
                            "name": "https:\/\/www.exploit-db.com\/exploits\/49563"
                        }
                    ],
                    "vulnerableSoftware": [
                        {
                            "software": {
                                "id": "cpe:2.3:a:tasks:tasks:*:*:*:*:*:*:*:*",
                                "vulnerabilityIdMatched": "true",
                                "versionEndExcluding": "9.7.3"
                            }
                        }
                    ]
                }
            ]
        },
        {
            "isVirtual": false,
            "fileName": "BufferedTokenStream.js",
            "filePath": "\/output\/input\/runtime\/JavaScript\/src\/antlr4\/BufferedTokenStream.js",
            "md5": "34584f5d7390826630f495b09e383e5e",
            "sha1": "1172009428e3492c85b900f493b59816510ce136",
            "sha256": "f177bd3df52ee6699553779a4ddfda931ac16e69bf30f4852fec1db0c005459a",
            "evidenceCollected": {
                "vendorEvidence": [],
                "productEvidence": [],
                "versionEvidence": []
            }
        },
-}
data DependencyCheckDependency =
  DependencyCheckDependency
    { _dcd_isVirtual         :: Bool
    , _dcd_fileName          :: FilePath
    , _dcd_filePath          :: FilePath
    , _dcd_evidenceCollected :: Map.Map String [DependencyCheckEvidence]
    , _dcd_packages          :: [DependencyCheckPackage]
    , _dcd_vulnerabilities   :: [A.Object]
    }
  deriving (Eq, Show)

instance A.FromJSON DependencyCheckDependency where
  parseJSON =
    A.withObject "DependencyCheckDependency" $ \v -> do
      DependencyCheckDependency <$> v A..: "isVirtual" <*> v A..: "fileName" <*>
        v A..: "filePath" <*>
        v A..: "evidenceCollected" <*>
        fmap
          (\case
             Just ps -> ps
             Nothing -> [])
          (v A..:? "packages") <*>
        fmap
          (\case
             Just ps -> ps
             Nothing -> [])
          (v A..:? "vulnerabilities")

dependencyCheckDependencyToPath ::
     DependencyCheckDependency -> (Maybe FilePath, FilePath)
dependencyCheckDependencyToPath (DependencyCheckDependency False _ fp _ _ _) =
  (Nothing, "/" FP.</> fp)
dependencyCheckDependencyToPath (DependencyCheckDependency True fn fp _ _ _) =
  let undoAppendix x [] _ = x
      undoAppendix x (y:ys) z =
        if ys `List.isPrefixOf` z
          then x ++ [y]
          else undoAppendix (x ++ [y]) ys z
      baseFP = "/" FP.</> (undoAppendix [] fp ('?' : fn))
   in (Just (baseFP ++ "/"), baseFP FP.</> fn)

dependencyCheckPackageToCoordinates :: DependencyCheckPackage -> Coordinates
dependencyCheckPackageToCoordinates (DependencyCheckPackage {_dcp_id = id}) =
  case id of
    Right purl -> purlToCoordinates purl
    Left raw -> Coordinates (Just (T.pack raw)) Nothing Nothing Nothing Nothing

evidenceToPurls :: Map.Map String [DependencyCheckEvidence] -> [Purl]
evidenceToPurls evidence =
  let findBestFromEvidences :: [DependencyCheckEvidence] -> Maybe String
      findBestFromEvidences []    = Nothing
      findBestFromEvidences (e:_) = Just $ _dce_value e
      vendor = "vendorEvidence" `Map.lookup` evidence >>= findBestFromEvidences
      product =
        "productEvidence" `Map.lookup` evidence >>= findBestFromEvidences
      version =
        "versionEvidence" `Map.lookup` evidence >>= findBestFromEvidences
   in maybeToList $
      fmap
        (\product' ->
           Purl Nothing Nothing vendor product' version Nothing Nothing)
        product

evidenceToPackages ::
     Map.Map String [DependencyCheckEvidence] -> [DependencyCheckPackage]
evidenceToPackages =
  map (\purl -> DependencyCheckPackage (Right purl) Nothing Nothing Nothing) .
  evidenceToPurls

dependencyCheckDependencyToOpossum :: DependencyCheckDependency -> IO Opossum
dependencyCheckDependencyToOpossum (dcd@DependencyCheckDependency { _dcd_isVirtual = isVirtual
                                                                  , _dcd_packages = packages
                                                                  , _dcd_vulnerabilities = vulnerabilities
                                                                  , _dcd_evidenceCollected = evidence
                                                                  }) =
  let (potentialFWC, fp) = dependencyCheckDependencyToPath dcd
      rs = fpToResources True fp
      baseOpossum =
        mempty
          { _resources = fpToResources True fp
          , _filesWithChildren = (Set.fromList . maybeToList) potentialFWC
          }
      dependencyCheckPackageToOpossum (package@DependencyCheckPackage {_dcp_url = url}) = do
        uuid <- randomIO
        let cs = dependencyCheckPackageToCoordinates package
            eas = ExternalAttribution_Source "Dependency-Check" 50
            ea =
              ExternalAttribution
                { _source = eas
                , _attributionConfidence = 50
                , _comment =
                    case vulnerabilities of
                      [] -> Nothing
                      vulnerabilities' ->
                        Just
                          (T.decodeUtf8
                             (B.toStrict (A.encodePretty vulnerabilities)))
                , _originId = Nothing
                , _coordinates = cs
                , _copyright = Nothing
                , _licenseName = Nothing
                , _licenseText = Nothing
                , _url = url
                , _criticality = Nothing
                , _flags = mempty {_isFollowUp = vulnerabilities /= []}
                }
            opossum =
              baseOpossum
                { _externalAttributions = Map.singleton uuid ea
                , _resourcesToAttributions = Map.singleton fp [uuid]
                , _externalAttributionSources =
                    mkExternalAttributionSources eas Nothing 40
                }
        return opossum
   in fmap
        (\case
           [] ->
             if isVirtual
               then mempty
               else baseOpossum
           os -> mconcat os) $
      mapM
        dependencyCheckPackageToOpossum
        (case packages of
           []        -> evidenceToPackages evidence
           packages' -> packages')

{-
{
    "reportSchema": "1.1",
    "scanInfo": {
        "engineVersion": "6.2.2",
        "dataSource": [
            {
                "name": "NVD CVE Checked",
                "timestamp": "2021-09-18T09:12:35"
            },
            {
                "name": "NVD CVE Modified",
                "timestamp": "2021-09-18T05:00:01"
            },
            {
                "name": "VersionCheckOn",
                "timestamp": "2021-09-18T09:12:35"
            }
        ]
    },
    "projectInfo": {
        "name": "",
        "reportDate": "2021-09-18T09:17:44.104464Z",
        "credits": {
            "NVD": "This report contains data retrieved from the National Vulnerability Database: http://nvd.nist.gov",
            "NPM": "This report may contain data retrieved from the NPM Public Advisories: https://www.npmjs.com/advisories",
            "RETIREJS": "This report may contain data retrieved from the RetireJS community: https://retirejs.github.io/retire.js/",
            "OSSINDEX": "This report may contain data retrieved from the Sonatype OSS Index: https://ossindex.sonatype.org"
        }
    },
    "dependencies": [
 -}
data DependencyCheckFile =
  DependencyCheckFile
    { _dcf_metadata     :: A.Value
    , _dcf_dependencies :: [DependencyCheckDependency]
    }
  deriving (Eq, Show)

instance A.FromJSON DependencyCheckFile where
  parseJSON =
    A.withObject "DependencyCheckFile" $ \v -> do
      scanInfo <- v A..: "scanInfo" :: A.Parser A.Value
      projectInfo <- v A..: "projectInfo" :: A.Parser A.Value
      DependencyCheckFile <$>
        return
          (A.object ["scanInfo" A..= scanInfo, "projectInfo" A..= projectInfo]) <*>
        v A..: "dependencies"

parseDependencyCheckBS :: B.ByteString -> IO Opossum
parseDependencyCheckBS bs =
  case (A.eitherDecode bs :: Either String DependencyCheckFile) of
    Right (DependencyCheckFile metadata dependencies) ->
      fmap
        (mempty {_metadata = Map.singleton "Dependency-Check" metadata} <>)
        (mconcat $ map dependencyCheckDependencyToOpossum dependencies)
    Left err -> do
      hPutStrLn IO.stderr err
      undefined -- TODO

parseDependencyCheckToOpossum :: FilePath -> IO Opossum
parseDependencyCheckToOpossum inputPath = do
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
  opossum <- B.readFile inputPath >>= parseDependencyCheckBS
  return (normaliseOpossum (baseOpossum <> opossum))
