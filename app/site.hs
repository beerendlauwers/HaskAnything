--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mconcat,mappend,(<>))
import           Hakyll
import           Hakyll.Web.Tags

-- For the custom tag / category stuff
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

-- For the tag extraction stuff
import           Data.List                       (nub,intersect)
import           Data.Maybe                      (catMaybes,fromJust)
import           Data.String                     (fromString)
import           Data.Aeson                      (encode)

-- For the library stuff
import qualified Data.Map                        as M
import           Control.Monad                   (liftM)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
        
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
        
    match "vendor/***" $ do
        route   idRoute
        compile copyFileCompiler
        
    -- As explained at http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags "content/*/*" (fromCapture "tags/*")
    
    categories <- buildCategories "content/*/*" (fromCapture "categories/*")
    
    libraries <- buildLibraries "content/*/*" (fromCapture "libraries/*")

    match "content/snippets/*" $ do
        route $ setExtension ""
        let ctx = addTags tags $ addCategories categories $ addLibraries libraries $ postCtx
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content/snippet.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            
    match "content/reddit-post/*" $ do
        route $ setExtension ""
        let ctx = addTags tags $ addCategories categories postCtx
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content/reddit-post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            
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
                >>= loadAndApplyTemplate "templates/categories.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                
    create ["json/tags.json"] $ do
        route idRoute
        compile $ do
            makeItem (encode $ getUniqueTags' tags)
            
    create ["json/libraries.json"] $ do
        route idRoute
        compile $ do
            makeItem (encode $ getUniqueTags' libraries)
            
    create ["json/categories.json"] $ do
        route idRoute
        compile $ do
            makeItem (encode $ getUniqueTags' categories)
            
    --create ["json/content/snippets"]
            
    -- TODO: use this with library tags and such
    create ["test.html"] $ do
        route idRoute
        compile $ do
            let ctx =
                    listField "categories" 
                              (
                               field "category" (return . fst . itemBody) <>
                               listFieldWith "tags" (field "tag" (return . itemBody)) (sequence . map makeItem . snd . itemBody)
                              ) 
                              (sequence [makeItem ("test",["tag1","tag2"]), makeItem ("test2",["tag2","tag3"])]) <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/testing.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
                
    match "ui/elements/*" $ compile templateCompiler
    
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext =
                    field "allcategories" (\_ -> renderTagList categories) <>
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


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
    
addCategories :: Tags -> Context String -> Context String
addCategories = extend "category"

addTags :: Tags -> Context String -> Context String
addTags = extend "tags"

addLibraries :: Tags -> Context String -> Context String
addLibraries = extendWith libraryTagsField "libraries"

extend :: String -> Tags -> Context String -> Context String
extend s tags = mappend (contentTagsField s tags)

extendWith f s tags = mappend (f s tags)

libraryTagsField = tagsFieldWith getLibraries simpleRenderLink mconcat

contentTagsField = 
    tagsFieldWith getTags simpleRenderLink mconcat
    
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag

getUniqueTags' :: Tags -> [String]
getUniqueTags' (Tags m _ _) = nub $ map fst m

matchTagsWithCategories' (Tags tags _ _) (Tags cats _ _) = matchTagsWithCategories tags cats

matchTagsWithCategories tags cats = 
 map f cats
  where 
  f (cat,paths) = (cat, nub $ catMaybes $ concatMap (\c -> map (find c) tags ) cats)
  find (cat,cpaths) (tag,tpaths) = 
   if length (intersect cpaths tpaths) > 0
    then Just tag
    else Nothing                       

-- Library stuff
getLibrary :: MonadMetadata m => Int -> Identifier -> m (Maybe String)
getLibrary n identifier = getMetadataField identifier ("library-" ++ show n)

getLibraries identifier = (liftM catMaybes) $ mapM (\(n,i) -> getLibrary n i) (zip [1..] (replicate 10 identifier))

buildLibraries :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildLibraries = buildTagsWith getLibraries