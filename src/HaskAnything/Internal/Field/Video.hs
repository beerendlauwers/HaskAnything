{-# LANGUAGE OverloadedStrings          #-}
module HaskAnything.Internal.Field.Video where

import           Hakyll
import           Data.List.Split               (splitOn)
import           Data.List                     (isInfixOf)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Data.Maybe                    (listToMaybe,fromMaybe,catMaybes)

-- Supported video URLs.
data SupportedVideoURL = Youtube T.Text | Vimeo T.Text | VimeoPlayer T.Text deriving (Eq,Show)

-- Guesses a supported video URL from a part of the URL.
toSupportedVideoURL :: T.Text -> Maybe SupportedVideoURL
toSupportedVideoURL url = (listToMaybe . catMaybes) $ map (\v -> if T.isInfixOf (fst v) url then Just (snd v $ url) else Nothing) mapping
 where
  mapping =
   [("youtube",Youtube)
   ,("player.vimeo.com",VimeoPlayer)
   ,("vimeo.com",Vimeo)
   ]

-- Generates some HTML to embed the video in the page.
generateEmbedding :: SupportedVideoURL -> T.Text
generateEmbedding supVidUrl = generateEmbeddingWidthHeight (Just "100%") Nothing supVidUrl

getVideoId :: SupportedVideoURL -> T.Text
getVideoId (Youtube url) = (head . drop 1 . T.splitOn "?v=") url
getVideoId (VimeoPlayer url) =  (last . T.splitOn "/") url
getVideoId (Vimeo url) = (last . T.splitOn "/") url

-- Function that actually generates the HTML. Youtube videos don't need a width and height.
-- The other videos (currently just Vimeo) do.
generateEmbeddingWidthHeight :: Maybe T.Text -> Maybe T.Text -> SupportedVideoURL -> T.Text
generateEmbeddingWidthHeight w h v = case v of
 (Youtube url) -> "<div class=\"youtube-fix\"><iframe " <> genWidth w <>  genHeight h <> "src=\"" <> youtubeEmbedUrl v <> "\" frameborder=\"0\" class=\"video\" allowfullscreen></iframe></div>"
 (VimeoPlayer url) -> "<div class=\"youtube-fix\"><iframe " <> genWidth w <>  genHeight h <> "src=\"" <> url <> "\" autoplay=\"false\" frameborder=\"0\" class=\"video\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe></div>"
 (Vimeo url) -> "<div class=\"youtube-fix\"><iframe " <> genWidth w <>  genHeight h <> "src=\"" <> vimeoEmbedUrl v <> "\" autoplay=\"false\" frameborder=\"0\" class=\"video\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe></div>"
 where
  genHeight, genWidth :: Maybe T.Text -> T.Text
  genHeight = maybe "" (\height -> "height=\"" `T.append` height `T.append` "\" ")
  genWidth = maybe "" (\width -> "width=\"" `T.append` width `T.append` "\" ")

-- Takes a Youtube uRL and extracts the video ID from it to create an embeddable URL.
youtubeEmbedUrl :: SupportedVideoURL -> T.Text
youtubeEmbedUrl = ("https://www.youtube.com/embed/" `T.append`) . getVideoId

-- Takes a Vimeo uRL and extracts the video ID from it to create an embeddable URL.
vimeoEmbedUrl :: SupportedVideoURL -> T.Text
vimeoEmbedUrl = ("https://player.vimeo.com/video/" `T.append`) . getVideoId

generateVideoEmbed :: Context T.Text
generateVideoEmbed = functionField "generateVideoEmbed" $ \args item ->
  case args of
    [url] -> do return (selectType url)
    [url,width,height] -> do return (selectTypeWidthHeight url width height)
    _   -> fail "generateVideoEmbed expects either a single URL or a URL plus width and height"
 where
  selectType url = maybe "" generateEmbedding (toSupportedVideoURL url)
  selectTypeWidthHeight url width height =
   case toSupportedVideoURL url of
    Just v -> generateEmbeddingWidthHeight (Just width) (Just height) v
    Nothing -> ""

generateVideoPreviewImage :: Context T.Text
generateVideoPreviewImage = functionField "generateVideoPreviewImage" $ \args item ->
  case args of
    [url] -> do return (generatePreviewImage url)
    _   -> fail "generateVideoPreviewImage expects a single URL"
 where
   generatePreviewImage url =
     case toSupportedVideoURL url of
       Just v -> generatePreviewImage' v
       Nothing -> ""
   generatePreviewImage' v@(Youtube url) =
     let id = getVideoId v
     in "https://i1.ytimg.com/vi/" `T.append` id `T.append` "/mqdefault.jpg"
