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
import           Data.List                       (nub)
import           Data.Aeson                      (encode)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
        
    -- As explained at http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags "content/*/*" (fromCapture "tags/*")
    
    categories <- buildCategories "content/*/*" (fromCapture "categories/*")

    match "content/snippets/*" $ do
        route $ setExtension ""
        let ctx = addTags tags $ addCategories categories postCtx
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content/snippet.html"    ctx
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
               
    tagsRules categories $ \category pattern -> do
        let title = "Content category: " ++ category

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
                
    create ["tags.json"] $ do
        route idRoute
        compile $ do
            makeItem (encode $ getUniqueTags' tags)
            
                
    create ["categories-overview"] $ do
        route idRoute
        compile $ do
            let indexContext =
                    field "allcategories" (\_ -> renderTagList categories) <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/categories-overview.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
                

    match "templates/**" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
    
addCategories :: Tags -> Context String -> Context String
addCategories c = mappend (categoryField "category" c)

addTags :: Tags -> Context String -> Context String
addTags tags = mappend (contentTagsField "tags" tags)

contentTagsField = 
    tagsFieldWith getTags simpleRenderLink mconcat
    
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag

getUniqueTags' :: Tags -> [String]
getUniqueTags' (Tags m _ _) = nub $ map fst m