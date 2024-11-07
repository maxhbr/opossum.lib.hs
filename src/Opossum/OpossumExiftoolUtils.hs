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

module Opossum.OpossumExiftoolUtils
  ( parseExiftoolToOpossum
  , parseExiftoolBS
  ) where

import           Opossum.Opossum
import           Opossum.OpossumUtils

import qualified Control.Monad.State      as MTL
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types         as A
import qualified Data.Aeson.Key           as AKey
import qualified Data.Aeson.KeyMap        as AKM
import qualified Data.ByteString.Lazy     as B
import           Data.List                (intercalate)
import qualified Data.List                as List
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, isJust, maybeToList)
import qualified Data.Maybe               as Maybe
import           Data.Monoid
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Distribution.Parsec      as SPDX
import qualified Distribution.SPDX        as SPDX
import qualified System.FilePath          as FP
import           System.IO                (Handle, hClose, hPutStrLn, stdout)
import qualified System.IO                as IO
import           System.Random            (randomIO)

import           Debug.Trace              (trace)

keysForName :: [AKey.Key]
keysForName =
  [ "InternalName"
  , "ProjectPropertyGroupAssemblyName"
  , "ProjectPropertyGroupRootNamespace"
  ]

keysForVendor :: [AKey.Key]
keysForVendor =
  ["ProjectItemGroupReferenceInclude", "ProjectItemGroupReferenceName"]

keysForVersion :: [AKey.Key]
keysForVersion =
  [ "ProjectPropertyGroupProductVersion"
  , "ProductVersion"
  , "FileVersionNumber"
  , "ProductVersionNumber"
  ]

keysForCopyright :: [AKey.Key]
keysForCopyright = ["LegalCopyright", "CompanyName"]

keysForComment :: [AKey.Key]
keysForComment = ["FileDescription", "Comments", "comment"]

findFirstForKeys :: [AKey.Key] -> A.Object -> Maybe T.Text
findFirstForKeys keys o =
  Maybe.listToMaybe $
  Maybe.mapMaybe
    (\f ->
       case f o of
         Just (A.String t) ->
           let tStrip = T.strip t
            in if T.null tStrip
                 then Nothing
                 else Just tStrip
         _ -> Nothing)
    (map AKM.lookup keys)

exiftoolEntryToEA :: A.Object -> Maybe ExternalAttribution
exiftoolEntryToEA o =
  let name = findFirstForKeys keysForName o
      vendor = findFirstForKeys keysForVendor o
      version = findFirstForKeys keysForVersion o
      coordinates = Coordinates Nothing vendor name version Nothing
      copyright = findFirstForKeys keysForCopyright o
      comment = findFirstForKeys keysForComment o
      source = ExternalAttribution_Source "Exiftool" 90
      isNotEmpty = isJust name || isJust version || isJust copyright
   in if isNotEmpty
        then Just $
             ExternalAttribution
               source
               90
               comment
               Nothing
               coordinates
               copyright
               Nothing
               Nothing
               Nothing
               Nothing
               mempty
        else Nothing

exiftoolEntryToOpossum :: (FilePath, A.Object) -> IO Opossum
exiftoolEntryToOpossum (fp, o) = do
  uuid <- randomIO
  return $
    case exiftoolEntryToEA o of
      Just ea ->
        (mempty
           { _resources = fpToResources True fp
           , _externalAttributions = Map.singleton uuid ea
           , _resourcesToAttributions = Map.singleton ("/" FP.</> fp) [uuid]
           })
      Nothing -> mempty

parseExiftoolBS :: B.ByteString -> IO Opossum
parseExiftoolBS bs =
  case (A.eitherDecode bs :: Either String (Map.Map FilePath A.Object)) of
    Right m -> mconcat $ map exiftoolEntryToOpossum (Map.assocs m)
    Left err -> do
      hPutStrLn IO.stderr err
      undefined -- TODO

parseExiftoolToOpossum :: FilePath -> IO Opossum
parseExiftoolToOpossum inputPath = do
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
  opossum <- B.readFile inputPath >>= parseExiftoolBS
  return (normaliseOpossum (baseOpossum <> opossum))
