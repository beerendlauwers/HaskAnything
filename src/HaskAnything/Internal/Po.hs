{-# LANGUAGE OverloadedStrings #-}
module HaskAnything.Internal.Po where

import Hakyll.Web.Template.Context (Context, constField, missingField)
import Hakyll.Core.Compiler (Compiler (..), getResourceFilePath,getResourceBody,makeItem, loadBody)
import Hakyll.Core.Item (Item (..))
import Hakyll.Core.Dependencies (Dependency(..))
import Hakyll.Core.Rules (Rules(..),rulesExtraDependencies,create)
import Hakyll.Core.Identifier.Pattern (fromCapture, Pattern(..))
import Hakyll.Core.Identifier (toFilePath, Identifier(..))
import Hakyll.Core.Metadata
import HaskAnything.Internal.Po.Parse (MsgDec (..), Msgid (..), po)
import           Control.Monad                   (foldM, forM_)
import Data.Monoid (Monoid (..))
import Control.Applicative ((<$>),(<*>),pure)
import Text.ParserCombinators.Parsec (parse)

translationSetFromMsgDec :: [MsgDec] -> Context String
translationSetFromMsgDec = foldl mappend missingField . map toField . filter emptyMsgDec
 where
  emptyMsgDec (MsgDec mctx msgid msgstrs) = (get msgid) == ""
  toField (MsgDec mctx msgid msgstrs) = constField (get msgid) (head msgstrs)
  get (Msgid s) = s
  

  
msgDecToTuple :: MsgDec -> (String,[String])
msgDecToTuple (MsgDec mctx (Msgid s) msgstrs) = (s,msgstrs)
  
readPo :: Compiler (Item [MsgDec])
readPo = do
 path <- getResourceFilePath
 str  <- getResourceBody
 let result = (parse po path) <$> str
 return $ get <$> result
  where
   get (Left e)  = []
   get (Right m) = m
   

data Translations = Translation
    { translationPatterns  :: [(String,Pattern)]
    , translationDependency :: Dependency
    } deriving (Show)
   
   {-
   
-- Add this to site.hs.
translationRules :: Translations -> (String -> Pattern -> (Context String) -> Rules ()) -> Rules ()
translationRules t rules = 
 forM_ (translationPatterns t) $ \(language,identifiers) ->
    rulesExtraDependencies [translationDependency t] $ do
        ids <- getMatches identifiers
        translations <- foldM addTranslations [] ids
        create [fromCapture "translations/*" language] $ 
            rules language identifiers (translationSetFromMsgDec translations)
  where addTranslations allmsgdecs identifier = do
            msgdecs <- readTranslation identifier
            return $ allmsgdecs ++ msgdecs
    -}
    {-
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags tag] $
                rules tag $ fromList identifiers
                -}
    
 -- rulesExtraDependencies takes a list of dependencies and adds them to all existing compilers *and* all future compilers via a fixed point computation.
 -- The Rules you pass are added with these dependencies as well.
   

   {- -- Broke since the move to Hakyll 4.7.4 ?
readTranslation :: Identifier -> Rules [MsgDec]
readTranslation i = do
  body <- loadBody i
  let result = parse po (toFilePath i) body
  return $ get result
  where
   get (Left e)  = []
   get (Right m) = m
   
   -}
 
 {-
 -- TODO: We can probably get the language name out of the PO parser, but for now let's add it manually.
buildTranslation :: String -> Pattern -> m Translation
buildTranslation langname p = do
 matches <- getMatches p
 terms <- foldM addTranslations [] matches
 return $ Translation terms (PatternDependency p matches) langname
  where addTranslations allmsgdecs identifier = do
            msgdecs <- readTranslation identifier
            return $ allmsgdecs ++ msgdecs
        
        -}
 
 
                {-
                buildTagsWith :: MonadMetadata m
              => (Identifier -> m [String])
              -> Pattern
              -> (String -> Identifier)
              -> m Tags
buildTagsWith f pattern makeId = do
    ids    <- getMatches pattern
    tagMap <- foldM addTags M.empty ids
    return $ Tags (M.toList tagMap) makeId (PatternDependency pattern ids)
  where
    -- Create a tag map for one page
    addTags tagMap id' = do
        tags <- f id'
        let tagMap' = M.fromList $ zip tags $ repeat [id']
        return $ M.unionWith (++) tagMap tagMap'
        
        -}