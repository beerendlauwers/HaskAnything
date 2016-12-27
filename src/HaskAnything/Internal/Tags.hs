{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.Tags where

import           Hakyll
import           Data.Monoid (mconcat,mappend)

-- For the custom tag / category stuff
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           System.FilePath                 (dropExtension)

-- For the library stuff
import qualified Data.Map                        as M
import           Control.Monad                   (liftM,mplus,forM_)

-- For the tag extraction stuff
import           Data.List                       (nub,intersect)
import           Data.Maybe                      (fromMaybe,catMaybes,fromJust)

-- The default tagsRules function doesn't allow me to set an extension on the created tag identifier, which is what I need.
tagsRules' :: Tags -> (String -> Pattern -> Rules ()) -> Rules ()
tagsRules' tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags (tag ++ ".html")] $ do
                rules tag $ fromList identifiers

-- |Adds the categories of a piece of content to the context.
addCategories :: Tags -> Context String -> Context String
addCategories = extend "category"

-- |Adds the tags of a piece of content to the context.
addTags :: Tags -> Context String -> Context String
addTags = extend "tags"

-- |Adds the libraries of a piece of content to the context.
addLibraries :: Tags -> Context String -> Context String
addLibraries = extend "libraries"

-- |Helper function for extending a context with some tags.
extend :: String -> Tags -> Context String -> Context String
extend s tags = mappend (contentTagsField s tags)

-- |Helper function for extending a context with some tags.
-- |You can supply a custom $f$ to define where
extendWith f s tags = mappend (f s tags)

contentTagsField =
    tagsFieldWith getTags simpleRenderLink mconcat

simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag

-- |Gives you a list of unique tags.
getUniqueTags' :: Tags -> [String]
getUniqueTags' (Tags m _ _) = nub $ map fst m


matchTagsWithCategories' :: Tags -> Tags -> [(String, [String])]
matchTagsWithCategories' (Tags tags _ _) (Tags cats _ _) = matchTagsWithCategories tags cats

-- |Given a list of tags and a list of categories, searches for common 'Identifier's in both and zips it up wih the category.
matchTagsWithCategories :: [(String, [Identifier])] -> [(String, [Identifier])] -> [(String, [String])]
matchTagsWithCategories tags cats =
 map f cats
  where
  f (cat,paths) = (cat, nub $ catMaybes $ concatMap (\c -> map (find c) tags ) cats)
  find (cat,cpaths) (tag,tpaths) =
   if length (intersect cpaths tpaths) > 0
    then Just tag
    else Nothing


getLibraries :: MonadMetadata m => Identifier -> m [String]
getLibraries identifier = do
    metadata <- getMetadata identifier
    return $ fromMaybe [] $
        (lookupStringList "libraries" metadata) `mplus`
        (map trim . splitAll "," <$> lookupString "libraries" metadata)

buildLibraries :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildLibraries = buildTagsWith getLibraries

getCategoryType :: MonadMetadata m => Identifier -> m [String]
getCategoryType identifier = do
    metadata <- getMetadata identifier
    return $ [ fromMaybe [] $ lookupString "type" metadata ]

buildCategoryTypes :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildCategoryTypes = buildTagsWith getCategoryType

getTitle :: MonadMetadata m => Identifier -> m [String]
getTitle identifier = do
   (return . (:[]) . dropExtension . toFilePath) identifier

buildPermissionFiles :: MonadMetadata m => Pattern -> (String-> Identifier) -> m Tags
buildPermissionFiles = buildTagsWith getTitle
