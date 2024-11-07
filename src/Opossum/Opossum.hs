-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeFamilies      #-}

module Opossum.Opossum
  ( Resources(..)
  , countFiles
  , fpToResources
  , fpsToResources
  , resourcesToPaths
  , isPathAFileInResources
  , FrequentLicense(..)
  , Coordinates(..)
  , purlToCoordinates
  , coordinatesAreNotNull
  , ExternalAttribution(..)
  , eaIsSignificant
  , ExternalAttribution_Source(..)
  , ExternalAttribution_Flags(..)
  , ExternalAttributionSources(..)
  , ExternalAttributionSourcesEntry(..)
  , mkExternalAttributionSources
  , justPreselectedFlags
  , justExcludeFromNoticeFlags
  , Opossum(..)
  , writeOpossumStats
  , module X
  ) where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types         as A
import qualified Data.Aeson.Key           as AKey
import qualified Data.Aeson.KeyMap        as AKM
import qualified Data.ByteString.Lazy     as B
import qualified Data.List                as List
import qualified Data.Map                 as Map
import           Data.Maybe               (isJust)
import qualified Data.Maybe               as Maybe
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import           Data.UUID                (UUID)
import qualified Data.Vector              as V
import           GHC.Generics
import           Purl.Purl                as X
import qualified System.FilePath          as FP
import           System.IO                (Handle, hClose, hPutStrLn, stdout)
import qualified System.IO                as IO
import qualified System.Process           as P
import           System.Random            (randomIO)

objectNoNulls :: [A.Pair] -> A.Value
objectNoNulls =
  let dropNulls []               = []
      dropNulls ((_, A.Null):ps) = dropNulls ps
      dropNulls (p:ps)           = p : (dropNulls ps)
   in A.object . dropNulls

data Resources =
  Resources
    { _dirs  :: Map.Map FilePath Resources
    , _files :: Set.Set FilePath
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON Resources where
  toJSON (Resources dirs files) =
    let pairsFromDirs =
          map
            (\(fragment, resources) ->
               ((AKey.fromString fragment) A..= (A.toJSON resources)))
            (Map.toList dirs)
        pairsFromFiles =
          map (\fragment -> (AKey.fromString fragment) A..= (1 :: Int)) $
          Set.toList files
     in A.object (pairsFromFiles ++ pairsFromDirs)

instance A.FromJSON Resources where
  parseJSON =
    A.withObject "Resources" $ \v ->
      let isObject :: A.Value -> Bool
          isObject (A.Object _) = True
          isObject _            = False
          getFiles :: [(FilePath, A.Value)] -> Set.Set FilePath
          getFiles =
            Set.fromList .
            map (\(fp, _) -> fp) . (filter (\(_, v') -> not (isObject v')))
          getDirs ::
               [(FilePath, A.Value)] -> A.Parser (Map.Map FilePath Resources)
          getDirs list = do
            parsedList <-
              mapM
                (\(fp, o) -> do
                   rs <- A.parseJSON o :: A.Parser Resources
                   return (fp, rs))
                (filter (\(_, v') -> isObject v') list)
            return (Map.fromList parsedList)
       in do let vList = map (\(t, v') -> (AKey.toString t, v')) $ AKM.toList v
             dirs <- getDirs vList
             return (Resources dirs (getFiles vList))

instance Semigroup Resources where
  (Resources dirs1 files1) <> (Resources dirs2 files2) =
    let dirs = (Map.unionWith (<>) dirs1 dirs2)
        dirNames = Map.keys dirs
        files = Set.filter (\f -> not $ f `elem` dirNames) $ files1 <> files2
     in Resources dirs files

instance Monoid Resources where
  mempty = Resources Map.empty Set.empty

fpToResources :: Bool -> FilePath -> Resources
fpToResources isFile =
  let fpToResources' :: [FilePath] -> Resources
      fpToResources' [] = Resources mempty mempty
      fpToResources' (f:[]) =
        if isFile
          then Resources (Map.empty) (Set.singleton f)
          else Resources (Map.singleton f mempty) Set.empty
      fpToResources' ("/":fs) = fpToResources' fs
      fpToResources' (f:fs) =
        Resources (Map.singleton f (fpToResources' fs)) Set.empty
   in fpToResources' .
      (map FP.dropTrailingPathSeparator) . FP.splitPath . FP.normalise

fpsToResources :: [FilePath] -> Resources
fpsToResources = mconcat . map (fpToResources True)

countFiles :: Resources -> Int
countFiles (Resources dirs files) =
  length files + ((sum . map countFiles . Map.elems) dirs)

resourcesToPaths :: Resources -> Set.Set FilePath
resourcesToPaths =
  let resourcesToPaths' :: Resources -> Set.Set [FilePath]
      resourcesToPaths' (Resources {_dirs = dirs, _files = files}) =
        let rsFromDirs =
              mconcat .
              map
                (\(k, rs) ->
                   Set.insert [k ++ "/"] . Set.map (k :) $ resourcesToPaths' rs) $
              Map.assocs dirs
            rsFromFiles = Set.map (: []) files
         in rsFromDirs <> rsFromFiles
   in Set.insert "/" . Set.map FP.joinPath . resourcesToPaths'

isPathAFileInResources :: FilePath -> Resources -> Bool
isPathAFileInResources =
  let isPathAFileInResources' :: [FilePath] -> Resources -> Bool
      isPathAFileInResources' [f] (Resources {_files = fs}) = f `Set.member` fs
      isPathAFileInResources' (f:fp) (Resources {_dirs = ds}) =
        case Map.lookup f ds of
          Just resources -> isPathAFileInResources' fp resources
          Nothing        -> True
   in \fp -> isPathAFileInResources' (FP.splitPath fp)

data ExternalAttribution_Source =
  ExternalAttribution_Source String Double
  deriving (Show, Generic, Eq)

instance A.ToJSON ExternalAttribution_Source where
  toJSON (ExternalAttribution_Source source documentConfidence) =
    A.object
      [ "name" A..= (T.pack source)
      , "documentConfidence" A..= documentConfidence
      ]

instance A.FromJSON ExternalAttribution_Source where
  parseJSON =
    A.withObject "ExternalAttribution_Source" $ \v -> do
      ExternalAttribution_Source <$> v A..: "name" <*>
        v A..: "documentConfidence"

data Coordinates =
  Coordinates
    { _packageType         :: Maybe T.Text
    , _packageNamespace    :: Maybe T.Text
    , _packageName         :: Maybe T.Text
    , _packageVersion      :: Maybe T.Text
    , _packagePurlAppendix :: Maybe T.Text
    }
  deriving (Show, Generic, Eq)

opoossumCoordinatesPreObjectList :: Coordinates -> [A.Pair]
opoossumCoordinatesPreObjectList (Coordinates packageType packageNamespace packageName packageVersion packagePurlAppendix) =
  [ "packageType" A..= packageType
  , "packageNamespace" A..= packageNamespace
  , "packageName" A..= packageName
  , "packageVersion" A..= packageVersion
  , "packagePurlAppendix" A..= packagePurlAppendix
  ]

instance A.ToJSON Coordinates where
  toJSON = objectNoNulls . opoossumCoordinatesPreObjectList

instance A.FromJSON Coordinates where
  parseJSON =
    A.withObject "Coordinates" $ \v -> do
      packageType <- v A..:? "packageType"
      packageNamespace <- v A..:? "packageNamespace"
      packageName <- v A..:? "packageName"
      packageVersion <- v A..:? "packageVersion"
      packagePurlAppendix <- v A..:? "packagePurlAppendix"
      return $
        Coordinates
          packageType
          packageNamespace
          packageName
          packageVersion
          packagePurlAppendix

purlToCoordinates :: Purl -> Coordinates
purlToCoordinates (purl@Purl { purlType = type_
                             , purlName = name
                             , purlVersion = version
                             }) =
  let
      packageNamespace = case purlNamespace purl of
        "" -> Nothing
        ns -> Just (T.pack ns)
      packageVersion = case version of
        "" -> Nothing
        v  -> Just (T.pack v)
    in Coordinates
          ((Just . T.pack) type_)
          packageNamespace
          (Just $ T.pack name)
          packageVersion
          Nothing -- TODO: appendix

coordinatesAreNotNull :: Coordinates -> Bool
coordinatesAreNotNull (Coordinates Nothing Nothing _ Nothing _) = False
coordinatesAreNotNull _                                         = True

data ExternalAttribution_Flags =
  ExternalAttribution_Flags
    { _isFirstParty        :: Bool
    , _isPreSelected       :: Bool
    , _isExcludeFromNotice :: Bool
    , _isFollowUp          :: Bool
    }
  deriving (Show, Generic, Eq)

opoossumExternalAttributionFlagsPreObjectList ::
     ExternalAttribution_Flags -> [A.Pair]
opoossumExternalAttributionFlagsPreObjectList flags =
  let flagToJSON :: (ExternalAttribution_Flags -> Bool) -> String -> [A.Pair]
      flagToJSON pred name =
        if pred flags
          then [(AKey.fromString name) A..= True]
          else []
   in concat
        [ flagToJSON _isFirstParty "firstParty"
        , flagToJSON _isPreSelected "preSelected"
        , flagToJSON _isExcludeFromNotice "excludeFromNotice"
        ] ++
      (if _isFollowUp flags
         then ["followUp" A..= ("FOLLOW_UP" :: String)]
         else [])

instance A.FromJSON ExternalAttribution_Flags where
  parseJSON =
    A.withObject "ExternalAttribution_Flags" $ \v ->
      let readFlag :: String -> A.Parser Bool
          readFlag name =
            fmap
              (\case
                 Just b  -> b
                 Nothing -> False)
              (v A..:? (AKey.fromString name))
          readStringFlag :: String -> A.Parser Bool
          readStringFlag name =
            fmap
              (\case
                 Just "" -> False
                 Just _  -> True
                 Nothing -> False)
              (v A..:? (AKey.fromString name) :: A.Parser (Maybe String))
       in ExternalAttribution_Flags <$> readFlag "firstParty" <*>
          readFlag "preSelected" <*>
          readFlag "excludeFromNotice" <*>
          readStringFlag "followUp"

instance Semigroup ExternalAttribution_Flags where
  (ExternalAttribution_Flags f1 f2 f3 f4) <> (ExternalAttribution_Flags f1' f2' f3' f4') =
    ExternalAttribution_Flags (f1 || f1') (f2 || f2') (f3 || f3') (f4 || f4')

instance Monoid ExternalAttribution_Flags where
  mempty = ExternalAttribution_Flags False False False False

justPreselectedFlags = mempty {_isPreSelected = True}

justExcludeFromNoticeFlags = mempty {_isExcludeFromNotice = True}

data ExternalAttribution =
  ExternalAttribution
    { _source                :: ExternalAttribution_Source
    , _attributionConfidence :: Double
    , _comment               :: Maybe T.Text
    , _originId              :: Maybe UUID
    , _coordinates           :: Coordinates
    , _copyright             :: Maybe T.Text
    , _licenseName           :: Maybe T.Text
    , _licenseText           :: Maybe T.Text
    , _url                   :: Maybe T.Text
    , _criticality           :: Maybe T.Text
    , _flags                 :: ExternalAttribution_Flags
    }
  deriving (Show, Generic, Eq)

instance A.ToJSON ExternalAttribution where
  toJSON (ExternalAttribution source attributionConfidence comment originId coordinates copyright licenseName licenseText url criticality flags) =
    objectNoNulls
      ([ "source" A..= source
       , "attributionConfidence" A..= attributionConfidence
       , "comment" A..= comment
       , "copyright" A..= copyright
       , "licenseName" A..= licenseName
       , "licenseText" A..= licenseText
       , "originId" A..= originId
       , "url" A..= url
       , "criticality" A..= criticality
       ] ++
       (opoossumCoordinatesPreObjectList coordinates) ++
       (opoossumExternalAttributionFlagsPreObjectList flags))

instance A.FromJSON ExternalAttribution where
  parseJSON =
    A.withObject "ExternalAttribution" $ \v -> do
      source <- v A..: "source"
      attributionConfidence <-
        fmap (100 `Maybe.fromMaybe`) (v A..:? "attributionConfidence")
      comment <- v A..:? "comment"
      originId <-
        fmap
          (fmap read .
           (\case
              Just "" -> Nothing
              x       -> x))
          (v A..:? "originId" :: A.Parser (Maybe String))
      coordinates <- A.parseJSON (A.Object v) :: A.Parser Coordinates
      copyright <- v A..:? "copyright"
      licenseName <-
        fmap
          (\case
             Just "" -> Nothing
             l       -> l)
          (v A..:? "licenseName")
      licenseText <-
        fmap
          (\case
             Just "" -> Nothing
             l       -> l)
          (v A..:? "licenseText")
      url <-
        fmap
          (\case
             Just "" -> Nothing
             l       -> l)
          (v A..:? "url")
      criticality <-
        fmap
          (\case
             Just "" -> Nothing
             l       -> l)
          (v A..:? "criticality")
      flags <- A.parseJSON (A.Object v) :: A.Parser ExternalAttribution_Flags
      return
        (ExternalAttribution
           source
           attributionConfidence
           comment
           originId
           coordinates
           copyright
           licenseName
           licenseText
           url
           criticality
           flags)

eaIsSignificant :: ExternalAttribution -> Bool
eaIsSignificant ExternalAttribution { _comment = comment
                                    , _coordinates = coordinates
                                    , _copyright = copyright
                                    , _licenseName = licenseName
                                    , _licenseText = licenseText
                                    , _url = url
                                    , _flags = flags
                                    } =
  or
    [ isJust comment
    , coordinatesAreNotNull coordinates
    , isJust copyright
    , isJust licenseText
    , isJust url
    , flags /= mempty
    ]

data FrequentLicense =
  FrequentLicense
    { shortName   :: T.Text
    , fullName    :: T.Text
    , defaultText :: T.Text
    }
  deriving (Eq, Show, Generic)

instance A.ToJSON FrequentLicense where
  toJSON (FrequentLicense sn fn dt) =
    A.object ["shortName" A..= sn, "fullName" A..= fn, "defaultText" A..= dt]

instance A.FromJSON FrequentLicense where
  parseJSON =
    A.withObject "FrequentLicense" $ \v -> do
      FrequentLicense <$> v A..: "shortName" <*> v A..: "fullName" <*>
        v A..: "defaultText"

data ExternalAttributionSourcesEntry =
  ExternalAttributionSourcesEntry String Integer
  deriving (Eq, Show, Generic)

instance A.FromJSON ExternalAttributionSourcesEntry where
  parseJSON =
    A.withObject "ExternalAttributionSourcesEntry" $ \v -> do
      ExternalAttributionSourcesEntry <$> v A..: "name" <*> v A..: "priority"

instance A.ToJSON ExternalAttributionSourcesEntry where
  toJSON (ExternalAttributionSourcesEntry name priority) =
    A.object ["name" A..= name, "priority" A..= priority]

newtype ExternalAttributionSources =
  ExternalAttributionSources (Map.Map String ExternalAttributionSourcesEntry)
  deriving (Eq, Show, Generic)

instance A.FromJSON ExternalAttributionSources where
  parseJSON = fmap ExternalAttributionSources . A.parseJSON

instance A.ToJSON ExternalAttributionSources where
  toJSON (ExternalAttributionSources eas) = A.toJSON eas

instance Semigroup ExternalAttributionSources where
  (ExternalAttributionSources eas1) <> (ExternalAttributionSources eas2) =
    ExternalAttributionSources (eas1 <> eas2)

instance Monoid ExternalAttributionSources where
  mempty = ExternalAttributionSources mempty

mkExternalAttributionSources ::
     ExternalAttribution_Source
  -> Maybe String
  -> Integer
  -> ExternalAttributionSources
mkExternalAttributionSources (ExternalAttribution_Source key _) name priority =
  ExternalAttributionSources
    (Map.singleton
       key
       (ExternalAttributionSourcesEntry (key `Maybe.fromMaybe` name) priority))

data Opossum =
  Opossum
    { _metadata                   :: Map.Map String A.Value
    , _resources                  :: Resources
    , _externalAttributions       :: Map.Map UUID ExternalAttribution
    , _resourcesToAttributions    :: Map.Map FilePath [UUID]
    , _attributionBreakpoints     :: Set.Set FilePath
    , _filesWithChildren          :: Set.Set FilePath
    , _frequentLicenses           :: [FrequentLicense]
    , _baseUrlsForSources         :: Map.Map FilePath String
    , _externalAttributionSources :: ExternalAttributionSources
    }
  deriving (Show, Generic)

instance A.ToJSON Opossum where
  toJSON (Opossum metadata resources externalAttributions resourcesToAttributions attributionBreakpoints filesWithChildren frequentLicenses baseUrlsForSources externalAttributionSources) =
    objectNoNulls
      [ "metadata" A..= metadata
      , "resources" A..= resources
      , "externalAttributions" A..= externalAttributions
      , "resourcesToAttributions" A..= resourcesToAttributions
      , "attributionBreakpoints" A..= attributionBreakpoints
      , "filesWithChildren" A..= filesWithChildren
      , "frequentLicenses" A..= frequentLicenses
      , "baseUrlsForSources" A..= baseUrlsForSources
      , "externalAttributionSources" A..= externalAttributionSources
      ]

instance A.FromJSON Opossum where
  parseJSON =
    A.withObject "Opossum" $ \v -> do
      let resourcesParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "resources"
          metadataParser = fmap (mempty `Maybe.fromMaybe`) $ v A..:? "metadata"
          externalAttributionsParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "externalAttributions"
          resourcesToAttributionsParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "resourcesToAttributions"
          attributionBreakpointsParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "attributionBreakpoints"
          filesWithChildrenParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "filesWithChildren"
          frequentLicensesParser =
            fmap
              (\case
                 Just fls -> fls
                 Nothing  -> [])
              (v A..:? "frequentLicenses")
          baseUrlsForSourcesParser =
            fmap (mempty `Maybe.fromMaybe`) $ v A..:? "baseUrlsForSources"
          externalAttributionSourcesParser =
            fmap (mempty `Maybe.fromMaybe`) $
            v A..:? "externalAttributionSources"
      Opossum <$> metadataParser <*> resourcesParser <*>
        externalAttributionsParser <*>
        resourcesToAttributionsParser <*>
        attributionBreakpointsParser <*>
        filesWithChildrenParser <*>
        frequentLicensesParser <*>
        baseUrlsForSourcesParser <*>
        externalAttributionSourcesParser

instance Semigroup Opossum where
  opossum1 <> opossum2 =
    let mergedMetadata = Map.union (_metadata opossum1) (_metadata opossum2)
        mergedResources = _resources opossum1 <> _resources opossum2
        mergedExternalAttributions =
          Map.union
            (_externalAttributions opossum1)
            (_externalAttributions opossum2)
        mergedResourcesToAttributions =
          Map.unionWith
            (++)
            (_resourcesToAttributions opossum1)
            (_resourcesToAttributions opossum2) -- TODO: nub
        mergedAttributionBreakpoints =
          (_attributionBreakpoints opossum1 <> _attributionBreakpoints opossum2) -- TODO: nub
        mergedFilesWithChildren =
          (_filesWithChildren opossum1 <> _filesWithChildren opossum2) -- TODO: nub
        mergedFrequentLicenses =
          List.nub (_frequentLicenses opossum1 ++ _frequentLicenses opossum2)
        mergedBaseUrlsForSources =
          Map.union
            (_baseUrlsForSources opossum1)
            (_baseUrlsForSources opossum2)
        mergedExternalAttributionsSources =
          (_externalAttributionSources opossum1) <>
          (_externalAttributionSources opossum2)
     in Opossum
          mergedMetadata
          mergedResources
          mergedExternalAttributions
          mergedResourcesToAttributions
          mergedAttributionBreakpoints
          mergedFilesWithChildren
          mergedFrequentLicenses
          mergedBaseUrlsForSources
          mergedExternalAttributionsSources

instance Monoid Opossum where
  mempty =
    Opossum mempty mempty mempty mempty mempty mempty mempty mempty mempty

writeOpossumStats :: Opossum -> IO ()
writeOpossumStats (Opossum { _metadata = m
                           , _resources = rs
                           , _externalAttributions = eas
                           , _resourcesToAttributions = rtas
                           , _attributionBreakpoints = abs
                           , _filesWithChildren = fwcs
                           , _frequentLicenses = fls
                           }) = do
  hPutStrLn IO.stderr ("metadata: " ++ show m)
  hPutStrLn IO.stderr ("resources: #files=" ++ show (countFiles rs))
  hPutStrLn IO.stderr ("externalAttributions: #=" ++ show (length eas))
  mapM_
    (\easOfType ->
       hPutStrLn
         IO.stderr
         ("externalAttributions of type '" ++
          head easOfType ++ "' #=" ++ show (length easOfType)))
    (List.groupBy
       (==)
       (List.sort $
        map ((\(ExternalAttribution_Source s _) -> s) . _source) (Map.elems eas)))
  hPutStrLn IO.stderr ("resourcesToAttributions: #=" ++ show (length rtas))
  hPutStrLn IO.stderr ("attributionBreakpoints: #=" ++ show (length abs))
  hPutStrLn IO.stderr ("filesWithChildren: #=" ++ show (length fwcs))
  hPutStrLn IO.stderr ("frequentLicenses: #=" ++ show (length fls))
