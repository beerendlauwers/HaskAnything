--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
import           Data.Monoid (mconcat,mappend,(<>))

import           Hakyll.Core.Metadata           (lookupString)
import           Hakyll
import           Hakyll.Web.Tags
import           Control.Applicative           (empty)
import           System.FilePath               (dropExtension, takeFileName, takeBaseName, takeDirectory)

import           HaskAnything.Internal.Content
import           HaskAnything.Internal.Tags
import           HaskAnything.Internal.Facet
import           HaskAnything.Internal.JSON
import           HaskAnything.Internal.Extra     (toString,loadBodyLBS,getCategory)
import           HaskAnything.Internal.Field     (urlReplaceField, getFieldFromMetadata, getManyFieldsFromMetaData, loadSeriesList, relativizeUrl)
import           HaskAnything.Internal.Field.Video (generateVideoEmbed)

import           HaskAnything.Internal.Po

import           Control.Monad                   (foldM, forM, mplus, join)

import           Data.Tuple.Utils

import           Data.Aeson                      (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                      (fromMaybe,catMaybes)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/***" $ do
        route   idRoute
        compile $ do getResourceBody >>= applyAsTemplate (relativizeUrl <> defaultContext)

    match "css/******" $ do
        route   idRoute
        compile compressCssCompiler

    match "vendor/***" $ do
        route   idRoute
        compile copyFileCompiler

    -- As explained at http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    -- fromCapture fills in the * in the first string with the second string provided (so it's partially applied here).
    -- It's used to create an identifier for the system.
    tags <- buildTags "content/*/*" (fromCapture "tags/*")

    categories <- buildCategories "content/*/*" (fromCapture "categories/*")

    libraries <- buildLibraries "content/*/*" (fromCapture "libraries/*")

    articleTypes <- buildCategoryTypes "content/article/*" (fromCapture "type/*")

    seriesTypes <- buildCategoryTypes "content/series/*" (fromCapture "type/*")

    matchContent "paper" (addTags tags $ addCategories categories $ addLibraries libraries $ postCtx tags categories libraries)
    matchContent "snippet" (addTags tags $ addCategories categories $ addLibraries libraries $ postCtx tags categories libraries)
    matchContent "reddit-post" (addTags tags $ addCategories categories $ postCtx tags categories libraries)
    matchContent "reddit-thread" (addTags tags $ addCategories categories $ postCtx tags categories libraries)
    matchContent "presentation" (addTags tags $ addCategories categories $ postCtx tags categories libraries)
    matchContent "series" (addTags tags $ addCategories categories $ seriesCtx tags categories libraries)
    matchContent "article" (addTags tags $ addCategories categories $ articleCtx tags categories libraries)
    matchContent "package" (addTags tags $ addCategories categories $ packageCtx tags categories libraries)
    matchContent "how-do-i/simple" (addTags tags $ addCategories categories $ postCtx tags categories libraries)
    matchContent "how-do-i/advanced" (addTags tags $ addCategories categories $ postCtx tags categories libraries)

    -- See https://hackage.haskell.org/package/hakyll-4.6.9.0/docs/Hakyll-Web-Tags.html
    tagsRules' tags $ \tag pattern -> do
        let title = "Content tagged with " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            alltags <- loadAll pattern
            let ctx = constField "title" title <>
                        listField "alltags" (addTags tags $ postCtx tags categories libraries) (return alltags) <>
                        defaultContext' tags categories libraries
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules' libraries $ \tag pattern -> do
        let title = "Content tagged with library " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            allLibraries <- loadAll pattern
            let ctx = constField "title" title <>
                        listField "alllibraries" (addTags tags $ postCtx tags categories libraries) (return allLibraries) <>
                        defaultContext' tags categories libraries
            makeItem ""
                >>= loadAndApplyTemplate "templates/libraries.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules' categories $ \category pattern -> do
        let title = "Content in category " ++ category

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            allCategories <- loadAll pattern
            let ctx = constField "title" title <>
                        listField "allcategories" (addTags tags $ addCategories categories $ postCtx tags categories libraries) (return allCategories) <>
                        defaultContext' tags categories libraries
            makeItem ""
                >>= addCategoryText category ctx
                >>= loadAndApplyTemplate "templates/categories.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    makeJSONFile "tags" tags
    makeJSONFile "libraries" libraries
    makeJSONFile "categories" categories
    makeJSONFile "article-types" articleTypes
    makeJSONFile "series-types" seriesTypes

    create ["filter.html"] $ do
        route idRoute
        compile $ do
            -- Load the content identifiers.
            allContent::[Item String] <- loadAll "content/*/*"
            let idents = map itemIdentifier allContent

            -- Get metadata from them.
            allCategories <- sequence (map getCategory idents)
            allMetadata <- sequence (map getMetadata idents)
            allRoutes <- sequence (map getRoute idents)

            -- Zip it up for easy access later on.
            let zipped = zip3 allMetadata allRoutes allCategories

            -- Construct a data structure that Hakyll's templating system understands.
            let allIdents =
                      listField "tagData"
                        (
                            field "title" (return . (\metadata -> fromMaybe "(no title)" $ lookupString "title" metadata) . fst3 . itemBody) <>
                            field "tags" (return . (processList "tags") . fst3 . itemBody) <>
                            field "libraries" (return . (processList "libraries") . fst3 . itemBody) <>
                            field "url" (return . (\route -> fromMaybe "#" route) . snd3 . itemBody) <>
                            field "category" (return . BSL.unpack . encode . thd3 . itemBody)
                        )
                        ( sequence (map makeItem zipped) )

            -- Add that data structure to our local compilation context.
            let ctx = allIdents <> constField "title" "Filter by facets" <> defaultContext' tags categories libraries
            makeItem ""
                >>= loadAndApplyTemplate "templates/filter.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    makeJSONFileFromMetadataInContent "content/*/*" "conference" "conferences"

    match "ui/elements/*" $ compile templateCompiler

    match "permissions/*/*" $ do
        route   $ setExtension "html"
        compile $ do
            pandocCompiler
            >>= loadAndApplyTemplate "templates/permissions.html" (defaultContext' tags categories libraries)
            >>= loadAndApplyTemplate "templates/default.html" (defaultContext' tags categories libraries)
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            howDoIPosts <- loadAll "content/how-do-i/*/*"
            dedup <- deduplicateContentBy getRoute (withFilePath takeFileName) howDoIPosts -- It might actually be nice if we filtered this and had two "how-do-i-posts" columns, one advanced, one simple?

            let indexContext =
                    listField "how-do-i-posts" (postCtx tags categories libraries) (return dedup ) <>
                    field "categories" (\_ -> return $ toString $ tagsToJSON categories) <>
                    field "allcategories" (\_ -> renderTagList categories) <>
                    field "tags" (\_ -> fmap toString (loadBodyLBS "json/categories.json")) <>
                   -- field "path" (\_ -> fmap fromJust (getRoute "categories/*")) <> -- fix the fromJust -- TODO: find out how we can get that.
                   defaultContext' tags categories libraries


            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    match "ui/submit/*" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate (defaultContext' tags categories libraries)
                >>= loadAndApplyTemplate "templates/submit.html" (defaultContext' tags categories libraries)
                >>= loadAndApplyTemplate "templates/default.html" (defaultContext' tags categories libraries)
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

    match "templates/*/*" $ do
        route idRoute
        compile $ do
            r <- templateCompiler
            makeItem (show (itemBody r))

    match "documentation.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (defaultContext' tags categories libraries)
            >>= relativizeUrls


--------------------------------------------------------------------------------
postCtx :: Tags -> Tags -> Tags -> Context String
postCtx t c l =
    urlReplaceField "url-to-advanced" ("simple","advanced") `mappend`
    urlReplaceField "url-to-simple"   ("advanced","simple") `mappend`
    githubUrl `mappend`
    generateVideoEmbed `mappend`
    defaultContext' t c l

defaultContext' :: Tags -> Tags -> Tags -> Context String
defaultContext' t c l = facetCtx  t c l <> categoryContext c <> defaultContext

categoryContext :: Tags -> Context String
categoryContext ts =
 listField "categoryContext"
 (
    field "categoryName" (return . itemBody )
 )
 (sequence $ map makeItem (map fst $ tagsMap ts))


facetCtx :: Tags -> Tags -> Tags -> Context String
facetCtx tags categories libraries =
 listField "facetList"
 (
     field "facetName" (return . facetName . itemBody) <>
     field "facetList" (return . facetList . itemBody) <>
     field "facetPrettyName" (return . facetPrettyName . itemBody) <>
     field "facetPath" (return . facetPath . itemBody)
 )
 (sequence $ map (\(nm,p,t) -> makeItem $ generateFacetList nm p t) [("Tags","tags",tags),("Categories","categories",categories),("Libraries","libraries",libraries)])

processList nm metadata = (BSL.unpack . encode) $
    case lookupString nm metadata of
        (Just s) -> (map trim . splitAll ",") s
        Nothing -> fromMaybe [] (lookupStringList nm metadata)

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

seriesCtx :: Tags -> Tags -> Tags -> Context String
seriesCtx t c l = loadSeriesList contexts <> ctx
 where
   ctx = postCtx t c l
   contexts =
     [
      ("article", articleCtx t c l),
      ("how-do-i", postCtx t c l),
      ("package", packageCtx t c l),
      ("paper", postCtx t c l),
      ("presentation", postCtx t c l),
      ("reddit-post", postCtx t c l),
      ("series", seriesCtx t c l)
     ]


articleCtx :: Tags -> Tags -> Tags -> Context String
articleCtx t c l = ifField "has-permission" (extractMetadata "permission-file") <> getManyFieldsFromMetaData ["authors","date","url","permission-file","blog"]  <> postCtx t c l

packageCtx :: Tags -> Tags -> Tags -> Context String
packageCtx t c l = getManyFieldsFromMetaData ["name","authors","source","hackage","stackage","synopsis"]  <> postCtx t c l

videoCtx :: Tags -> Tags -> Tags -> Context String
videoCtx t c l = getManyFieldsFromMetaData ["url-video","url-slides","authors","source"]  <> postCtx t c l

githubUrl :: Context String
githubUrl = field "githubUrl" $ return . ("https://github.com/beerendlauwers/HaskAnything/edit/master/app/" ++) . toFilePath  . itemIdentifier

withFilePath :: (FilePath -> String) -> Maybe FilePath ->  String
withFilePath pathTostr mbFilePath =
    case mbFilePath of
      Nothing       -> "withFilePath ???"
      Just filePath -> pathTostr filePath
