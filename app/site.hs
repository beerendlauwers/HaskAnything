--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
import           Data.Monoid (mconcat,mappend,(<>))

import           Hakyll.Core.Metadata           (lookupString)
import           Hakyll
import           Hakyll.Web.Tags
import           Control.Applicative           (empty)
import           System.FilePath               (dropExtension, takeFileName, takeBaseName, takeDirectory)

import           HaskAnything.Internal.Content
import           HaskAnything.Internal.Context
import           HaskAnything.Internal.Tags
import           HaskAnything.Internal.Facet (addCategoryText)
import           HaskAnything.Internal.JSON
import           HaskAnything.Internal.Extra     (toString,loadBodyLBS,getCategory)
import           HaskAnything.Internal.Field     (relativizeUrl,getContentData)

import           HaskAnything.Internal.Po

import           Control.Monad                   (foldM, forM, mplus, join)

import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Maybe                      (fromMaybe)


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

    match "files/***" $ do
        route   idRoute
        compile copyFileCompiler

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

    permissionFiles <- buildPermissionFiles "permissions/*/*" (fromCapture "permissions/*")

    authors <- buildAuthors "content/*/*" (fromCapture "authors/*")

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
    makeJSONFile "permission-files" permissionFiles
    makeJSONFile "authors" authors

    create ["filter.html"] $ do
        route idRoute
        compile $ do

            -- Get all the content data as a list field.
            allIdents <- getContentData

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
            -- Get all the content data as a list field.
            allIdents <- getContentData

            getResourceBody
                >>= applyAsTemplate (allIdents <> defaultContext' tags categories libraries)
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

withFilePath :: (FilePath -> String) -> Maybe FilePath ->  String
withFilePath pathTostr mbFilePath =
    case mbFilePath of
      Nothing       -> "withFilePath ???"
      Just filePath -> pathTostr filePath
