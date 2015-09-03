--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mconcat,mappend,(<>))
import           Hakyll
import           Hakyll.Web.Tags

import           HaskAnything.Internal.Content
import           HaskAnything.Internal.Tags
import           HaskAnything.Internal.Facet
import           HaskAnything.Internal.JSON
import           HaskAnything.Internal.Extra     (toString,loadBodyLBS)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
        
    match "js/***" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/******" $ do
        route   idRoute
        compile compressCssCompiler
        
    match "vendor/***" $ do
        route   idRoute
        compile copyFileCompiler
        
    -- As explained at http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags "content/*/*" (fromCapture "tags/*")
    
    categories <- buildCategories "content/*/*" (fromCapture "categories/*")
    
    libraries <- buildLibraries "content/*/*" (fromCapture "libraries/*")
    
    matchContent "snippet" (addTags tags $ addCategories categories $ addLibraries libraries $ postCtx)
    matchContent "reddit-post" (addTags tags $ addCategories categories postCtx)
    matchContent "reddit-thread" (addTags tags $ addCategories categories postCtx)
            
    -- See https://hackage.haskell.org/package/hakyll-4.6.9.0/docs/Hakyll-Web-Tags.html
    tagsRules tags $ \tag pattern -> do
        let title = "Content tagged with " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            alltags <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "alltags" (addTags tags postCtx) (return alltags) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                
    tagsRules libraries $ \tag pattern -> do
        let title = "Content tagged with library " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            allLibraries <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "alllibraries" (addTags tags postCtx) (return allLibraries) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/libraries.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
               
    tagsRules categories $ \category pattern -> do
        let title = "Content in category " ++ category

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            allCategories <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "allcategories" (addTags tags $ addCategories categories $ postCtx) (return allCategories) <>
                        defaultContext
            makeItem ""
                >>= addCategoryText category ctx
                >>= loadAndApplyTemplate "templates/categories.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
    
    tagsToJSON "tags" tags
    tagsToJSON "libraries" libraries
    tagsToJSON "categories" categories
                
    match "ui/elements/*" $ compile templateCompiler
    
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext =
                    field "allcategories" (\_ -> renderTagList categories) <>
                    field "tags" (\_ -> fmap toString (loadBodyLBS "json/categories.json")) <>
                   -- field "path" (\_ -> fmap fromJust (getRoute "categories/*")) <> -- fix the fromJust -- TODO: find out how we can get that.
                    listField "facetList"
                                (
                                    field "facetName" (return . facetName . itemBody) <>
                                    field "facetList" (return . facetList . itemBody) <>
                                    field "facetPrettyName" (return . facetPrettyName . itemBody) <>
                                    field "facetPath" (return . facetPath . itemBody)
                                )
                                (sequence $ map (\(nm,p,t) -> makeItem $ generateFacetList nm p t) [("Tags","tags",tags),("Categories","categories",categories),("Libraries","libraries",libraries)]) <>
                   defaultContext
                                    

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
                
    match "ui/submit/*" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler
    
    match "templates/*/*" $ do
        route idRoute
        compile $ do
            r <- templateCompiler
            makeItem (show (itemBody r))

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
                       
