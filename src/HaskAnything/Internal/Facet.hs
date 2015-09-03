{-# LANGUAGE DeriveDataTypeable #-}

module HaskAnything.Internal.Facet where

import           Hakyll

import           HaskAnything.Internal.Extra     (toString)
import           HaskAnything.Internal.Tags      (getUniqueTags')

-- Facet stuff
import           Data.Char                       (toLower)
import           Data.Aeson                      (encode)
import           Data.Data

-- |A 'ContentFacet' is a way to discover content.
-- |For example, content could reside in a particular category: that's a facet of the content.
data ContentFacet = 
 ContentFacet { -- | A '[String]' encoded in JSON format. We place this in some Javascript code. See the generated index.html file to see how it works.
                facetList :: String
                -- | The relative path (from the point of view of the index.html file) to the facet. For example, all tags can be found in "/tags".
              , facetPath :: String
                -- | Javascript-compatible name for the facet.
              , facetName :: String
                -- | HTML-compatible name for the facet.
              , facetPrettyName :: String
              } deriving (Show,Data,Typeable)
              
-- |Converts a list of 'Tags' into a 'ContentFacet'.
generateFacetList :: String -> String -> Tags -> ContentFacet
generateFacetList nm p t = 
 ContentFacet { facetList = toString $ encode $ getUniqueTags' t
              , facetPath = p -- TODO: something nicer?
              , facetName = (map toLower nm)
              , facetPrettyName = nm
              }
              
-- |A category is just the folder within which some content resides.
-- |It's not content by itself, so we can't add metadata to it.
-- |We select a template with some text and apply it before the actual content.
-- |Another idea could have been to build up a 'Context' with category metadata and pass it along a generic template.
addCategoryText category = loadAndApplyTemplate (getTpl category)
 where 
    getTpl x = fromFilePath ("templates/categories/" ++ x ++ ".html")