{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.JSON where

import           Hakyll

import           Data.Aeson                      (encode)
import           HaskAnything.Internal.Tags      (getUniqueTags')
import           Data.List                       (nub)
import           Data.Maybe                      (fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as BSL

makeJSONFile name tags = do
    create [fromFilePath ("json/" ++ name ++ ".json")] $ do
        route idRoute
        compile $ do
            makeItem (tagsToJSON tags)

tagsToJSON tags = encode $ getUniqueTags' tags

makeJSONFileFromMetadataInContent :: Pattern -> String -> String -> Rules ()
makeJSONFileFromMetadataInContent pattern metadataField name = do
    create [fromFilePath ("json/" ++ name ++ ".json")] $ do
        route idRoute
        compile $ do
            allContent <- loadAll pattern :: Compiler [Item String]
            allMetadata <- sequence ( (map (getMetadataFromItem metadataField)) allContent)
            makeItem (encode $ nub $ filter (/="") allMetadata)

getMetadataFromItem :: String -> Item a -> Compiler String
getMetadataFromItem metadataField i = do
 m <- getMetadataField (itemIdentifier i) metadataField
 return $ maybe [] id m

-- Takes a metadata field name, looks it up in the metadata
-- and expects back a list. This list is then turned into JSON and returned
-- as a String.
processList :: String -> Metadata -> String
processList nm metadata = (BSL.toString . encode) $ lookupInMetadata nm metadata

lookupInMetadata :: String -> Metadata -> [String]
lookupInMetadata nm metadata = case lookupString nm metadata of
    (Just s) -> (map trim . splitAll ",") s
    Nothing -> fromMaybe [] (lookupStringList nm metadata)
