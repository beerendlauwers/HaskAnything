{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module HaskAnything.Internal.Field where

import           Hakyll
import           Data.Data
import           Data.List                     (find)
import           Data.Maybe                    (fromMaybe)
import           Data.String.Utils             (replace)
import           Control.Applicative           (empty)
import           HaskAnything.Internal.Extra   (getCategory)

urlReplaceField :: String -> (String,String) -> Context a
urlReplaceField fieldName (old,new) = field fieldName $ \item -> do
        mbFilePath <- getRoute (itemIdentifier item)
        case mbFilePath of
            Nothing       -> return "urlReplaceField: ???"
            Just filePath -> return $ toUrl $ replace old new $ filePath

getFieldFromMetadata :: String -> Context String
getFieldFromMetadata key = field key (\i -> fmap (maybe empty id) (getMetadataField  (itemIdentifier i) key) )

getManyFieldsFromMetaData :: [String] -> Context String
getManyFieldsFromMetaData keys = foldr1 mappend (map getFieldFromMetadata keys)

loadSeriesList :: [(String,Context String)] -> Context b
loadSeriesList contextMap = Context $ \k _ i ->
  if k == "seriesList"
    then
      do
        -- Load the metadata from the item.
        metadata <- getMetadata (itemIdentifier i)
        -- Get the values under the "list" key as a list of strings.
        let listItems = (fromMaybe [] (lookupStringList "list" metadata)) :: [String]
        -- Turn those strings into a list of valid Identifiers.
        let filePaths = (map (\p -> fromFilePath ("content/" ++ p)) listItems) :: [Identifier]
        -- Use the list of Identifiers to load them up as Items.
        loadedItems <- (mapM load filePaths) :: Compiler [Item String]
        -- Fetch the category of the first item. We will use that in our
        -- ListField constructor. Will have to work something out for
        -- ListFields with multiple context support.
        categories <- mapM (getCategory) filePaths
        -- Return a listField with the key "loadedItems" and loadedItems.
        return (ListField (getContext ((head.head) categories)) loadedItems)
    else empty
 where
   getContext cat =
     case find ((== cat) . fst) contextMap of
       (Just c) -> snd c
       otherwise -> error ("Could not find a context for category '" ++ cat ++ "' in loadSeriesList.")
