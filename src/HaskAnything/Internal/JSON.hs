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