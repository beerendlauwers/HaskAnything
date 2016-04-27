--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import           Data.Monoid (mconcat,mappend,(<>))
import qualified Data.List                      as L
import           Hakyll
import           Hakyll.Web.Tags
import           Control.Applicative           (empty)
import           Data.List.Split               (splitOn)

import           HaskAnything.Internal.Content
import           HaskAnything.Internal.Tags
import           HaskAnything.Internal.Facet
import           HaskAnything.Internal.JSON
import           HaskAnything.Internal.Extra     (toString,loadBodyLBS)

import           HaskAnything.Internal.Po

import           Control.Monad                   (foldM, forM, forM_, mplus)

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
    tags <- buildTags "content/*/*" (fromCapture "tags/*")
    
    categories <- buildCategories "content/*/*" (fromCapture "categories/*")
    
    libraries <- buildLibraries "content/*/*" (fromCapture "libraries/*")
    
    matchContent "snippet" (addTags tags $ addCategories categories $ addLibraries libraries $ postCtx)
    matchContent "reddit-post" (addTags tags $ addCategories categories postCtx)
    matchContent "reddit-thread" (addTags tags $ addCategories categories postCtx)
    matchContent "presentation" (addTags tags $ addCategories categories postCtx)
    matchContent "package" (addTags tags $ addCategories categories packageCtx)

    -- See https://hackage.haskell.org/package/hakyll-4.6.9.0/docs/Hakyll-Web-Tags.html
    tagsRules' tags $ \tag pattern -> do
        let title = "Content tagged with " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            alltags <- loadAll pattern
            let ctx = constField "title" title <>
                        listField "alltags" (addTags tags postCtx) (return alltags) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                
    tagsRules' libraries $ \tag pattern -> do
        let title = "Content tagged with library " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            allLibraries <- loadAll pattern
            let ctx = constField "title" title <>
                        listField "alllibraries" (addTags tags postCtx) (return allLibraries) <>
                        defaultContext
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
                        listField "allcategories" (addTags tags $ addCategories categories $ postCtx) (return allCategories) <>
                        defaultContext
            makeItem ""
                >>= addCategoryText category ctx
                >>= loadAndApplyTemplate "templates/categories.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
    
    makeJSONFile "tags" tags
    makeJSONFile "libraries" libraries
    makeJSONFile "categories" categories
    {- WIP
    match "translations/*" $ do
        compile readPo
    
    create "test.html" $ do
        route idRoute
        compile $ do
            let testContext = -}
            
            -- Interesting: https://github.com/yogsototh/yblog/blob/master/site.hs
            
                
    match "ui/elements/*" $ compile templateCompiler
    
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext =
                    field "categories" (\_ -> return $ toString $ tagsToJSON categories) <>
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
    githubUrl `mappend`
    generateVideoEmbed `mappend`
    defaultContext

-- The default tagsRules function doesn't allow me to set an extension on the created tag identifier, which is what I need.
tagsRules' :: Tags -> (String -> Pattern -> Rules ()) -> Rules ()
tagsRules' tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags (tag ++ ".html")] $ do
                rules tag $ fromList identifiers
    
getFieldFromMetadata :: String -> Context String
getFieldFromMetadata key = field key (\i -> fmap (maybe empty id) (getMetadataField  (itemIdentifier i) key) )
    
getManyFieldsFromMetaData :: [String] -> Context String
getManyFieldsFromMetaData keys = foldr1 mappend (map getFieldFromMetadata keys)
    
packageCtx :: Context String
packageCtx = getManyFieldsFromMetaData ["name","authors","source","hackage","stackage","synopsis"]  <> postCtx

videoCtx :: Context String 
videoCtx = getManyFieldsFromMetaData ["url-video","url-slides","authors","source"]  <> postCtx

githubUrl :: Context String
githubUrl = field "githubUrl" $ return . ("https://github.com/beerendlauwers/HaskAnything/edit/master/app/" ++) . toFilePath  . itemIdentifier
            
generateVideoEmbed :: Context String
generateVideoEmbed = functionField "generateVideoEmbed" $ \args item -> 
  case args of
    [url] -> do return (selectType url)
    [url,width,height] -> do return (selectTypeWidthHeight url width height)
    _   -> fail "generateVideoEmbed expects either a single URL or a URL plus width and height"
 where 
  youtubeEmbedUrl url = (("https://www.youtube.com/embed/" ++) . head . drop 1 . splitOn "?v=") url
  selectType url = 
   if L.isInfixOf "youtube" url 
   then ("<div class=\"youtube-fix\"><iframe width=\"100%\" src=\"" ++ youtubeEmbedUrl url ++ "\" frameborder=\"0\" class=\"video\" allowfullscreen></iframe></div>")
   else ""
  selectTypeWidthHeight url width height = 
   if L.isInfixOf "youtube" url 
   then  ("<iframe width=\"" ++ width ++ "\" height=\"" ++ height ++ "\" src=\"" ++ youtubeEmbedUrl url ++ "\" class=\"video\" frameborder=\"0\" allowfullscreen></iframe>")
   else ""
          
                       
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
        isRel x = "/" `L.isPrefixOf` x && not ("//" `L.isPrefixOf` x)
        rel x root = if isRel x then root ++ x else x
