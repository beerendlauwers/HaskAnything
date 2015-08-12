--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mconcat,mappend,(<>))
import           Hakyll
import           Hakyll.Web.Tags
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


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

    match "content/snippets/*" $ do
        route $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content/snippet.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls
            
    categories <- buildCategories "content/*/*" (fromCapture "categories/*")
            
    -- See https://hackage.haskell.org/package/hakyll-4.6.9.0/docs/Hakyll-Web-Tags.html
    tagsRules tags $ \tag pattern -> do
        let title = "Content tagged with " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            alltags <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "alltags" (postCtxWithTags tags) (return alltags) <>
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
                        listField "allcategories" (postCtxWithCategory categories) (return allCategories) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/categories.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                
    create ["categories"] $ do
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
    
postCtxWithTags :: Tags -> Context String 
postCtxWithTags tags = contentTagsField "tags" tags `mappend` postCtx

contentTagsField = 
    tagsFieldWith getTags simpleRenderLink mconcat

postCtxWithCategory :: Tags -> Context String 
postCtxWithCategory categories = categoryField "allcategories" categories `mappend` postCtx
    
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag" $ toHtml tag