{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, DeriveDataTypeable #-}

module HaskAnything.Internal.Field where

import           Hakyll
import           Data.Data
import           Data.List                     (find,isPrefixOf)
import           Data.Maybe                    (fromMaybe)
import           Data.String.Utils             (replace)
import           Control.Applicative           (empty)
import           HaskAnything.Internal.Extra   (getCategory)
import           Data.Monoid ((<>))
import           System.FilePath               (dropExtension)

urlReplaceField :: String -> (String,String) -> Context a
urlReplaceField fieldName (old,new) = field fieldName $ \item -> do
        mbFilePath <- getRoute (itemIdentifier item)
        case mbFilePath of
            Nothing       -> return "urlReplaceField: ???"
            Just filePath -> return $ toUrl $ replace old new $ filePath


pathToHTML :: Context a
pathToHTML = field "pathToHTML" $ \item -> do
    (return . dropExtension . toFilePath . itemIdentifier) item


-- Just appends the strings it's given and returns the result.
appendStrings :: Context a
appendStrings =  functionField "appendStrings" $ \args item ->
 return $ concat args

-- This is also in hakyll-extra, have to hook it up to this project.
relativizeUrl :: Context a
relativizeUrl = functionField "relativizeUrl" $ \args item ->
    case args of
        [k] -> do   route <- getRoute $ itemIdentifier item
                    return $ case route of
                        Nothing -> k
                        Just r -> rel k (toSiteRoot r)
        _   -> fail "relativizeUrl only needs a single argument"
     where
        isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
        rel x root = if isRel x then root ++ x else x

-- Given a key and a value, constructs a context that takes a key that will come from a Hakyll template.
-- If the key (k) coming from the Hakyll template corresponds with the one we provided (key), we return the value.
-- Otherwise, we return empty, which will make Hakyll continue down the monoidal context chain in search of another Context that could provide
-- a value for this key.
ifField :: String -> (Item a -> Compiler ContextField) -> Context a
ifField key value = Context $ \k _ i -> if k == key then value i else empty

-- Given the key of some metadata, extracts the value as a string and returns it.
-- If the metadata doesn't exist, we return empty.
extractMetadata :: String -> Item a -> Compiler ContextField
extractMetadata key i = do
 m <- getMetadataField (itemIdentifier i) key
 case m of
  Just x -> return (StringField x)
  Nothing -> empty

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
        let firstCategory = (head.head) categories
        let categoryField = constField ("seriesCategory." ++ firstCategory) firstCategory
        -- Return a listField with the key "loadedItems" and loadedItems.
        return (ListField (categoryField <> getContext firstCategory) loadedItems)
    else empty
 where
   getContext cat =
     case find ((== cat) . fst) contextMap of
       (Just c) -> snd c
       otherwise -> error ("Could not find a context for category '" ++ cat ++ "' in loadSeriesList.")
