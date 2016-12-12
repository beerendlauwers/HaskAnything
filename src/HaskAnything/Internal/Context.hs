module HaskAnything.Internal.Context where

import           Hakyll
import           Data.Monoid (mappend,(<>))
import           HaskAnything.Internal.Field     (urlReplaceField, extractMetadata, getManyFieldsFromMetaData, loadSeriesList, relativizeUrl, ifField, appendStrings,pathToHTML)
import           HaskAnything.Internal.Field.Video (generateVideoEmbed,generateVideoPreviewImage)
import           HaskAnything.Internal.Facet

postCtx :: Tags -> Tags -> Tags -> Context String
postCtx t c l =
    urlReplaceField "url-to-advanced" ("simple","advanced") `mappend`
    urlReplaceField "url-to-simple"   ("advanced","simple") `mappend`
    githubUrl `mappend`
    generateVideoEmbed `mappend`
    generateVideoPreviewImage `mappend`
    relativizeUrl `mappend`
    appendStrings `mappend`
    pathToHTML `mappend`
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
