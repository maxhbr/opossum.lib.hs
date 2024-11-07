-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Opossum.OpossumScancodeUtils
  ( parseScancodeToOpossum
  , parseScancodeBS
  , ScancodeFile(..)
  , ScancodeFileEntry(..)
  , scancodeFileEntryToEA
  , ScancodePackage(..)
  , scancodePackageToEA
  , parseScanpipeToOpossum
  , parseScanpipeBS
  , ScanpipeFile(..)
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
import           Data.Maybe               (fromMaybe, isJust, mapMaybe,
                                           maybeToList)
import           Data.Monoid
import qualified Data.Set                 as Set
import           Data.String              (fromString)
import qualified Data.Text                as T
import qualified Data.UUID.V5             as UUID
import qualified Data.Vector              as V
import qualified Distribution.Parsec      as SPDX
import qualified Distribution.SPDX        as SPDX
import           SPDX.Document            as SPDX
import qualified System.FilePath          as FP
import           System.IO                (Handle, hClose, hPutStrLn, stdout)
import qualified System.IO                as IO
import           System.Random            (randomIO)
import           Text.Printf              (printf)
import qualified Data.Functor

renderLicense :: MaybeLicenseExpression -> Maybe T.Text
renderLicense licenses =
  case licenses of
    (SPDX.MLicExp (SPDX.SPDXJust _)) -> (Just . T.pack . show) licenses
    _                                -> Nothing

{-
    {
      "path": "Cargo.lock",
      "type": "file",
      "name": "Cargo.lock",
      "base_name": "Cargo",
      "extension": ".lock",
      "size": 35114,
      "date": "2021-01-18",
      "sha1": "c3f1ca217637c6edec185df11cfdddbba1d3cac3",
      "md5": "c78cc22b4e1d5e6dade34aeb592c73f9",
      "sha256": "c2a6153229d3aad680248b0cbedf9cbc3cbfff5586be360e5f7efbb8dceca8cc",
      "mime_type": "text/plain",
      "file_type": "ASCII text",
      "programming_language": null,
      "is_binary": false,
      "is_text": true,
      "is_archive": false,
      "is_media": false,
      "is_source": false,
      "is_script": false,
      "licenses": [],
      "license_expressions": [],
      "percentage_of_license_text": 0,
      "copyrights": [],
      "holders": [],
      "authors": [],
      "packages": [
        {
          "type": "cargo",
          "namespace": null,
          "name": null,
          "version": null,
          "qualifiers": {},
          "subpath": null,
          "primary_language": "Rust",
          "description": null,
          "release_date": null,
          "parties": [],
          "keywords": [],
          "homepage_url": null,
          "download_url": null,
          "size": null,
          "sha1": null,
          "md5": null,
          "sha256": null,
          "sha512": null,
          "bug_tracking_url": null,
          "code_view_url": null,
          "vcs_url": null,
          "copyright": null,
          "license_expression": null,
          "declared_license": null,
          "notice_text": null,
          "root_path": null,
          "dependencies": [
            {
              "purl": "pkg:crates/adler@0.2.3",
              "requirement": "0.2.3",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
            {
              "purl": "pkg:crates/aho-corasick@0.7.15",
              "requirement": "0.7.15",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
...
          ],
          "contains_source_code": null,
          "source_packages": [],
          "purl": null,
          "repository_homepage_url": null,
          "repository_download_url": null,
          "api_data_url": null
        }
-}
data ScancodePackage =
  ScancodePackage
    { _scp_purl         :: Maybe PURL
    , _scp_licenses     :: SPDX.MaybeLicenseExpression
    , _scp_copyright    :: Maybe String
    , _scp_dependencies :: [ScancodePackage]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodePackage where
  parseJSON =
    A.withObject "ScancodePackage" $ \v -> do
      purl <-
        v A..:? "purl" >>=
        (\case
           Just purl -> return $ parsePURL purl
           Nothing   -> return Nothing)
      dependencies <-
        (v A..:? "dependencies" >>=
         (\case
            Just dependencies -> mapM A.parseJSON dependencies
            Nothing           -> return [])) :: A.Parser [ScancodePackage]
      license <- fmap (fromMaybe mempty) $ v A..:? "license_expression"
      copyright <- v A..:? "copyright"
      return $ ScancodePackage purl license copyright dependencies

data ScancodeFileEntry =
  ScancodeFileEntry
    { _scfe_file       :: FilePath
    , _scfe_is_file    :: Bool
    , _scfe_license    :: SPDX.MaybeLicenseExpression
    , _scfe_copyrights :: [String]
    , _scfe_packages   :: [ScancodePackage]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodeFileEntry where
  parseJSON =
    A.withObject "ScancodeFileEntry" $ \v -> do
      path <- v A..: "path"
      is_file <-
        (\case
           ("file" :: String) -> True
           _                  -> False) <$>
        v A..: "type"
    -- sha1 <- v `getHash` "sha1"
    -- md5 <- v `getHash` "md5"
    -- sha256 <- v `getHash` "sha256"
    -- sha512 <- v `getHash` "sha512"
    -- let idFromHashes = mconcat $ sha1 ++ md5 ++ sha256 ++ sha512
      let applyAll = appEndo . mconcat . map Endo
      licenseTransformator <-
        do licenseObjects <- (v A..: "licenses" :: A.Parser [A.Object])
           licenseNameTuples <-
             mapM
               (\v' -> do
                  key <- v' A..: "key" :: A.Parser T.Text
                  spdxkey <-
                    v' A..:? "spdx_license_key" :: A.Parser (Maybe T.Text)
                  return (key, spdxkey))
               licenseObjects
           (return .
            (\fun -> T.unpack . fun . T.pack) .
            applyAll .
            map (\(k1, Just s1) -> T.replace k1 s1) .
            List.sortBy
              (\ (k1, _) (k2, _) -> T.length k1 `compare` T.length k2) .
            filter (\(_, spdxkey) -> isJust spdxkey))
             licenseNameTuples
      license <-
        (v A..:? "license_expressions") Data.Functor.<&>
         (\case
            Just lics -> mconcat (map (fromString . licenseTransformator) lics)
            Nothing   -> SPDX.MLicExp SPDX.NOASSERTION)
      copyrights <-
        do listOfCopyrightObjects <-
             (v A..:? "copyrights" :: A.Parser (Maybe A.Array))
           case listOfCopyrightObjects of
             Just cos ->
               let getValueFromCopyrightObject =
                     A.withObject "CopyrightsEntry" $ \v' ->
                       v' A..: "value" :: A.Parser String
                in mapM getValueFromCopyrightObject (V.toList cos)
             Nothing -> return []
      packages <-
        (\case
           Just ps -> ps
           Nothing -> []) <$>
        v A..:? "packages"
      return (ScancodeFileEntry path is_file license copyrights packages)

data ScancodeFile =
  ScancodeFile
    { _scf_metadata :: A.Value
    , _scf_files    :: [ScancodeFileEntry]
    }
  deriving (Eq, Show)

instance A.FromJSON ScancodeFile where
  parseJSON =
    A.withObject "ScancodeFile" $ \v -> ScancodeFile <$> v A..: "headers" <*> v A..: "files"

scancodePackageToEA :: ScancodePackage -> Maybe ExternalAttribution
scancodePackageToEA scp@(ScancodePackage { _scp_purl = purl
                                         , _scp_licenses = licenses
                                         , _scp_copyright = copyright
                                         , _scp_dependencies = dependencies
                                         }) =
  let source = ExternalAttribution_Source "Scancode-Package" 50
      coordinatesFromPurl =
        case purl of
          Just purl -> purlToCoordinates purl
          _         -> Coordinates Nothing Nothing Nothing Nothing Nothing
   in Just $
      ExternalAttribution
        source
        50
        Nothing
        Nothing
        coordinatesFromPurl
        (fmap T.pack copyright)
        (renderLicense licenses)
        Nothing
        Nothing
        Nothing
        justPreselectedFlags

opossumFromScancodePackage :: ScancodePackage -> Maybe FilePath -> IO Opossum
opossumFromScancodePackage scp@(ScancodePackage { _scp_purl = purl
                                                , _scp_dependencies = dependencies
                                                }) providedPath =
  let typeFromPurl =
        case purl of
          Just (PURL {_PURL_type = t}) -> maybe "generic" show t
          _                            -> "generic"
      pathFromPurl =
        typeFromPurl FP.</>
        case purl of
          Just (PURL {_PURL_namespace = ns, _PURL_name = n, _PURL_version = v}) ->
            foldl1 (FP.</>) $
            maybeToList ns ++ [intercalate "@" $ n : maybeToList v]
          _ -> "UNKNOWN"
      path = fromMaybe pathFromPurl providedPath
   in do uuid <- randomIO
         let o =
               (case scancodePackageToEA scp of
                  Just ea ->
                    mempty
                      { _resources = fpToResources True path
                      , _externalAttributions = Map.singleton uuid ea
                      , _resourcesToAttributions =
                          Map.singleton ("/" FP.</> path) [uuid]
                      , _attributionBreakpoints =
                          case providedPath of
                            Just _ -> mempty
                            Nothing ->
                              Set.singleton ("/" ++ typeFromPurl ++ "/")
                      , _externalAttributionSources =
                          mkExternalAttributionSources (_source ea) Nothing 30
                      }
                  Nothing -> mempty)
         os <- mapM (`opossumFromScancodePackage` Nothing) dependencies
         return $ mconcat (o : map (unshiftPathToOpossum path) os)

scancodeFileEntryToEA :: ScancodeFileEntry -> Maybe ExternalAttribution
scancodeFileEntryToEA scfe@ScancodeFileEntry { _scfe_license = licenses
                                              , _scfe_copyrights = copyrights
                                              } =
  let source = ExternalAttribution_Source "Scancode" 50
      hasLicenses = case licenses of
                     (SPDX.MLicExp (SPDX.SPDXJust _)) -> True
                     _                                -> False
      hasCopyrights = not (null copyrights)
   in if hasLicenses || hasCopyrights
        then Just $
             ExternalAttribution
               source
               50
               Nothing
               Nothing
               (Coordinates Nothing Nothing Nothing Nothing Nothing)
               ((Just . T.pack . unlines) copyrights)
               (renderLicense licenses)
               Nothing
               Nothing
               Nothing
               mempty
        else Nothing

scancodeFileEntryToOpossum :: ScancodeFileEntry -> IO Opossum
scancodeFileEntryToOpossum scfe@(ScancodeFileEntry { _scfe_file = path
                                                   , _scfe_is_file = is_file
                                                   , _scfe_license = licenses
                                                   , _scfe_copyrights = copyrights
                                                   , _scfe_packages = packages
                                                   }) =
  let filesWithChildren =
        if is_file
          then Set.singleton ("/" FP.</> path ++ "/")
          else mempty
      opossumFromLicenseAndCopyright = do
        uuid <- randomIO
        let resources = fpToResources True path
        case scancodeFileEntryToEA scfe of
          Just ea -> do
            let eas = mkExternalAttributionSources (_source ea) Nothing 30
            return $
              mempty
                { _resources = resources
                , _externalAttributions = Map.singleton uuid ea
                , _resourcesToAttributions =
                    Map.singleton ("/" FP.</> path) [uuid]
                , _filesWithChildren = filesWithChildren
                , _externalAttributionSources = eas
                }
          Nothing ->
            return $
            mempty
              {_resources = resources, _filesWithChildren = filesWithChildren}
   in do o <- opossumFromLicenseAndCopyright
         oFromPackages <-
           case packages of
             [] -> mempty
             [p] -> opossumFromScancodePackage p (Just path)
             _ ->
               mconcat <$> mapM (`opossumFromScancodePackage` Nothing) packages
         return $ o <> oFromPackages

parseScancodeBS :: B.ByteString -> IO Opossum
parseScancodeBS bs =
  case (A.eitherDecode bs :: Either String ScancodeFile) of
    Right (ScancodeFile metadata scFiles) ->
      fmap
        (mempty {_metadata = Map.singleton "ScanCode" metadata} <>)
        (mconcat $ map scancodeFileEntryToOpossum scFiles)
    Left err -> do
      hPutStrLn IO.stderr err
      undefined -- TODO

parseScancodeToOpossum :: FilePath -> IO Opossum
parseScancodeToOpossum inputPath = do
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
  opossum <- B.readFile inputPath >>= parseScancodeBS
  return (normaliseOpossum (baseOpossum <> opossum))

data ScanpipeLayer =
  ScanpipeLayer
    { _spl_sha256             :: String
    , _spl_layer_id           :: String
    , _spl_created_by         :: String
    , _spl_archive_location   :: FilePath
    , _spl_extracted_location :: FilePath
    }
  deriving (Eq, Show)

instance A.FromJSON ScanpipeLayer where
  parseJSON =
    A.withObject "ScanpipeLayer" $ \v -> ScanpipeLayer <$> v A..: "sha256" <*> v A..: "layer_id" <*>
    v A..: "created_by" <*>
    v A..: "archive_location" <*>
    v A..: "extracted_location"

data ScanpipeFileEntry =
  ScanpipeFileEntry
    { _spfe             :: ScancodeFileEntry
    , _spfe_forPackages :: [String]
    , _spfe_status      :: String
    }
  deriving (Eq, Show)

instance A.FromJSON ScanpipeFileEntry where
  parseJSON =
    A.withObject "ScanpipeFileEntry" $ \v ->
      ScanpipeFileEntry <$> A.parseJSON (A.Object v) <*> v A..: "for_packages" <*>
      v A..: "status"

data ScanpipePackage =
  ScanpipePackage
    { _spp     :: ScancodePackage
    , _spp_key :: String
    }
  deriving (Eq, Show)

instance A.FromJSON ScanpipePackage where
  parseJSON =
    A.withObject "ScanpipePackage" $ \v -> ScanpipePackage <$> A.parseJSON (A.Object v) <*> v A..: "purl"

data ScanpipeFile =
  ScanpipeFile
    { _spf_metadata :: A.Value
    , _spf_layers   :: [ScanpipeLayer]
    , _spf_packages :: [ScanpipePackage]
    , _spf_files    :: [ScanpipeFileEntry]
    }
  deriving (Eq, Show)

instance A.FromJSON ScanpipeFile where
  parseJSON =
    A.withObject "ScanpipeFile" $ \v -> do
      let layersParser =
            v A..: "headers" >>=
            (\case
               header:_ ->
                 header A..: "extra_data" >>= (A..: "images") >>=
                 (\case
                    image:_ -> image A..: "layers"
                    _       -> return [])
               _ -> return [])
      ScanpipeFile <$> v A..: "headers" <*> layersParser <*> v A..: "packages" <*>
        v A..: "files"

layerPathReworkFun :: [ScanpipeLayer] -> FilePath -> FilePath
layerPathReworkFun layers =
  let tuples :: [(FilePath, FilePath)]
      tuples =
        (zipWith (\i -> (, printf "Layer_%03d" (i :: Int))) [1 ..] .
         map _spl_extracted_location)
          layers
      layerPathReworkFun' :: [(FilePath, FilePath)] -> FilePath -> FilePath
      layerPathReworkFun' [] input = input
      layerPathReworkFun' ((el, rp):oTuples) input =
        case List.stripPrefix el input of
          Just stripped -> rp ++ stripped
          Nothing       -> layerPathReworkFun' oTuples input
   in layerPathReworkFun' tuples

scanpipeLayerToEA :: ScanpipeLayer -> ExternalAttribution
scanpipeLayerToEA (ScanpipeLayer {_spl_created_by = cmd}) =
  let source = ExternalAttribution_Source "Scanpipe-Layer" 0
   in ExternalAttribution
        source
        50
        (Just (T.pack cmd))
        Nothing
        (Coordinates Nothing Nothing Nothing Nothing Nothing)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        justExcludeFromNoticeFlags

convertSPFToOpossum :: ScanpipeFile -> Opossum
convertSPFToOpossum spf@(ScanpipeFile _ spLayers spPackages spFiles) =
  let reworkPath = layerPathReworkFun spLayers
      eas0 =
        (Map.fromList .
         map
           (\spl@ScanpipeLayer {_spl_layer_id = layer_id} ->
              (uuidFromString layer_id, scanpipeLayerToEA spl)))
          spLayers
      eas1 =
        (Map.fromList .
         mapMaybe
           (\(ScanpipePackage scp key) ->
              case scancodePackageToEA scp of
                Just ea ->
                  Just (uuidFromString key, ea {_flags = justPreselectedFlags})
                Nothing -> Nothing))
          spPackages
      eas2 =
        (Map.fromList .
         mapMaybe
           (\(ScanpipeFileEntry scfe _ _) ->
              case scancodeFileEntryToEA scfe of
                Just ea ->
                  Just (uuidFromString (_scfe_file scfe ++ "scanpipefile"), ea)
                Nothing -> Nothing))
          spFiles
      rtas =
        Map.fromListWith (++) (map
            (\(ScanpipeFileEntry (ScancodeFileEntry {_scfe_file = file}) ps _) ->
               ( reworkPath file
               , uuidFromString (file ++ "scanpipefile") :
                 map uuidFromString ps))
            spFiles ++ map
            (\(ScanpipeLayer { _spl_layer_id = layer_id
                             , _spl_extracted_location = file
                             }) -> (reworkPath file, [uuidFromString layer_id]))
            spLayers)
      resources =
        fpsToResources
        (map (reworkPath . _scfe_file . _spfe) spFiles ++ Map.keys rtas)
   in mempty
        { _resources = resources
        , _externalAttributions = eas0 <> eas1 <> eas2
        , _resourcesToAttributions = rtas
            --  , _externalAttributionSources = undefined
        }

parseScanpipeBS :: B.ByteString -> IO Opossum
parseScanpipeBS bs =
  case (A.eitherDecode bs :: Either String ScanpipeFile) of
    Right spf@(ScanpipeFile metadata _ _ _) ->
      return
        (mempty {_metadata = Map.singleton "Scanpipe" metadata} <>
           convertSPFToOpossum spf)
    Left err -> do
      hPutStrLn IO.stderr err
      undefined -- TODO

parseScanpipeToOpossum :: FilePath -> IO Opossum
parseScanpipeToOpossum inputPath = do
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
  opossum <- B.readFile inputPath >>= parseScanpipeBS
  return (normaliseOpossum (baseOpossum <> opossum))
