{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

module HaskAnything.Internal.Field where

import           Hakyll
import           Data.Data
import           Data.List                     (find,isPrefixOf)
import           Data.Maybe                    (fromMaybe)
import           Data.String.Utils             (replace)
import           Control.Applicative           (empty)
import           HaskAnything.Internal.Extra   (getCategory)
import           HaskAnything.Internal.JSON    (processList,lookupInMetadata)
import           Data.Monoid ((<>))
import           System.FilePath               (dropExtension)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- For the content field generation fucntion
import           Data.Tuple.Utils
import           Data.Aeson                      (encode)

urlReplaceField :: T.Text -> (T.Text,T.Text) -> Context a
urlReplaceField fieldName (old,new) = field fieldName $ \item -> do
        mbFilePath <- getRoute (itemIdentifier item)
        case mbFilePath of
            Nothing       -> return "urlReplaceField: ???"
            Just filePath -> return $ T.pack $ toUrl $ T.unpack $ T.replace old new (T.pack filePath)


pathToHTML :: Context a
pathToHTML = field "pathToHTML" $ \item -> do
    (return . T.pack . dropExtension . toFilePath . itemIdentifier) item

-- Given two strings (could also be a field name that resolves to a string),
-- compares them and returns the result. Can be used in $if$ statements.
fieldEquals :: Context a
fieldEquals = functionField "fieldEquals" $ \args item ->
    case length args of
        2   -> case (head args) == (last args) of
                False -> empty
                True  -> return "ok"
        _   -> fail "fieldEquals requires two arguments."

fieldAsList :: Context a
fieldAsList = Context $ \k args i ->
  if k == "fieldAsList"
    then
      case args of
        [k] -> do
                -- Load the metadata from the item.
                metadata <- getMetadata (itemIdentifier i)
                -- Get the values under the key as a list of strings.
                let list = (fromMaybe [] (lookupStringList (head args) metadata)) :: [T.Text]
                listItems <- mapM makeItem list
                -- Return a listField with the key "loadedItems" and loadedItems.
                return (ListField (field "item" (return.itemBody)) listItems)
        _ -> fail "fieldAsList requires one argument. Be sure to pass in the field name as a string, like so: \"name\""
    else empty

-- Just appends the strings it's given and returns the result.
appendStrings :: Context a
appendStrings =  functionField "appendStrings" $ \args item ->
 return $ T.concat args

-- This is also in hakyll-extra, have to hook it up to this project.
relativizeUrl :: Context a
relativizeUrl = functionField "relativizeUrl" $ \args item ->
    case args of
        [k] -> do   route <- getRoute $ itemIdentifier item
                    return $ case route of
                        Nothing -> k
                        Just r -> T.pack $ rel (T.unpack k) (toSiteRoot r)
        _   -> fail "relativizeUrl only needs a single argument"
     where
        isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
        rel x root = if isRel x then root ++ x else x

-- Given a key and a value, constructs a context that takes a key that will come from a Hakyll template.
-- If the key (k) coming from the Hakyll template corresponds with the one we provided (key), we return the value.
-- Otherwise, we return empty, which will make Hakyll continue down the monoidal context chain in search of another Context that could provide
-- a value for this key.
ifField :: T.Text -> (Item a -> Compiler ContextField) -> Context a
ifField key value = Context $ \k _ i -> if k == key then value i else empty

-- Given the key of some metadata, extracts the value as a string and returns it.
-- If the metadata doesn't exist, we return empty.
extractMetadata :: T.Text -> Item a -> Compiler ContextField
extractMetadata key i = do
 m <- getMetadataField (itemIdentifier i) key
 case m of
  Just x -> return (StringField x)
  Nothing -> empty

getFieldFromMetadata :: T.Text -> Context T.Text
getFieldFromMetadata key = field key $ \i -> do
 f <- getMetadataField  (itemIdentifier i) key
 return $ fromMaybe T.empty f

getManyFieldsFromMetaData :: [T.Text] -> Context T.Text
getManyFieldsFromMetaData keys = foldr1 mappend (map getFieldFromMetadata keys)

loadSeriesList :: [(T.Text,Context T.Text)] -> Context b
loadSeriesList contextMap = Context $ \k _ i ->
  if k == "seriesList"
    then
      do
        -- Load the metadata from the item.
        metadata <- getMetadata (itemIdentifier i)
        -- Get the values under the "list" key as a list of strings.
        let listItems = (fromMaybe [] (lookupStringList "list" metadata)) :: [T.Text]
        -- Turn those strings into a list of valid Identifiers.
        let filePaths = (map (\p -> fromFilePath ("content/" ++ T.unpack p)) listItems) :: [Identifier]
        -- Use the list of Identifiers to load them up as Items.
        loadedItems <- (mapM load filePaths) :: Compiler [Item T.Text]
        -- Fetch the category of the first item. We will use that in our
        -- ListField constructor. Will have to work something out for
        -- ListFields with multiple context support.
        categories <- mapM (getCategory) filePaths
        let firstCategory = (head.head) categories
        let categoryField = constField ("seriesCategory." <> firstCategory) firstCategory
        -- Return a listField with the key "loadedItems" and loadedItems.
        return (ListField (categoryField <> getContext firstCategory) loadedItems)
    else empty
 where
   getContext cat =
     case find ((== cat) . fst) contextMap of
       (Just c) -> snd c
       otherwise -> error ("Could not find a context for category '" ++ (T.unpack cat) ++ "' in loadSeriesList.")

getMetadataListField :: MonadMetadata m => Identifier -> T.Text -> m (Maybe [T.Text])
getMetadataListField identifier key = do
   metadata <- getMetadata identifier
   return $ lookupStringList key metadata

--------------------------------------------------------------------------------
-- | Map any list field to its metadata value, if present
metadataListField :: Context a
metadataListField = Context $ \k _ i -> do
   values <- getMetadataListField (itemIdentifier i) k
   case values of
     Just vs -> do
                 listItems <- mapM makeItem vs
                 return $ ListField (field "item" (return.itemBody)) listItems
     Nothing -> empty

getContentData :: Compiler (Context a)
getContentData = do
  -- Load the content identifiers.
  allContent::[Item T.Text] <- loadAll "content/*/*"
  let idents = map itemIdentifier allContent

  -- Get metadata from them.
  allCategories <- sequence (map getCategory idents)
  allMetadata <- sequence (map getMetadata idents)
  allRoutes <- sequence (map getRoute idents)

  -- Zip it up for easy access later on.
  let zipped :: [(Metadata, Maybe T.Text, [T.Text])]
      zipped = zip3 allMetadata (map (T.pack <$>) allRoutes) allCategories

  -- Construct a data structure that Hakyll's templating system understands.
  let allIdents =
        listField "tagData"
          (
              field "title" (return . (\metadata -> fromMaybe "(no title)" $ lookupString "title" metadata) . fst3 . itemBody) <>
              field "tags" (return . (processList "tags") . fst3 . itemBody) <>
              field "authors" (return . (\metadata -> TL.toStrict . TL.decodeUtf8 . encode $ (lookupInMetadata "authors" metadata ++ lookupInMetadata "author" metadata)) . fst3 . itemBody) <>
              field "libraries" (return . (processList "libraries") . fst3 . itemBody) <>
              field "url" (return . (\route -> fromMaybe "#" route) . snd3 . itemBody) <>
              field "category" (return . TL.toStrict . TL.decodeUtf8 . encode . thd3 . itemBody)
          )
          ( sequence (map makeItem zipped) )

  return allIdents
