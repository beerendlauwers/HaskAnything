{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.JSON where

import           Hakyll

import           Data.Aeson                      (encode)
import           HaskAnything.Internal.Tags      (getUniqueTags')

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
            makeItem (encode $ filter (/="") allMetadata)
            
getMetadataFromItem :: String -> Item a -> Compiler String
getMetadataFromItem metadataField i = do
 m <- getMetadataField (itemIdentifier i) metadataField
 return $ maybe [] id m