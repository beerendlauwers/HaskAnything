{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.JSON where

import           Hakyll

import qualified Data.Aeson as Aeson             (encode)
import           HaskAnything.Internal.Tags      (getUniqueTags')
import           Data.List                       (nub)
import           Data.Maybe                      (fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

makeJSONFile name tags = do
    create [fromFilePath ("json/" ++ name ++ ".json")] $ do
        route idRoute
        compile $ do
            makeItem (tagsToJSON tags)

tagsToJSON tags = Aeson.encode $ getUniqueTags' tags

makeJSONFileFromMetadataInContent :: Pattern -> T.Text -> T.Text -> Rules ()
makeJSONFileFromMetadataInContent pattern metadataField name = do
    create [fromFilePath ("json/" ++ T.unpack name ++ ".json")] $ do
        route idRoute
        compile $ do
            allContent <- loadAll pattern :: Compiler [Item T.Text]
            allMetadata <- sequence ( (map (getMetadataFromItem metadataField)) allContent)
            makeItem (Aeson.encode $ nub $ filter (/="") allMetadata)

getMetadataFromItem :: T.Text -> Item a -> Compiler T.Text
getMetadataFromItem metadataField i = do
 m <- getMetadataField (itemIdentifier i) metadataField
 return $ maybe T.empty id m

-- Takes a metadata field name, looks it up in the metadata
-- and expects back a list. This list is then turned into JSON and returned
-- as a String.
processList :: T.Text -> Metadata -> T.Text
processList nm metadata = (TL.toStrict . TL.decodeUtf8 . Aeson.encode) $ lookupInMetadata nm metadata

lookupInMetadata :: T.Text -> Metadata -> [T.Text]
lookupInMetadata nm metadata = case lookupString nm metadata of
    (Just s) -> (map T.strip . T.splitOn ",") s
    Nothing -> fromMaybe [] (lookupStringList nm metadata)
