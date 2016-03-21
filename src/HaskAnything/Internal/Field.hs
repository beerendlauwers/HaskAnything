{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module HaskAnything.Internal.Field where

import           Hakyll
import           Data.Data

class HakyllField a where
    toHakyllField :: String -> a -> Context b
    
instance HakyllField Int where
    toHakyllField key = field key . const . return . show
    
instance HakyllField String where
    toHakyllField = constField
    
data MyTestData = Testing { greetings :: String, testingthis :: Int } deriving (Data,Typeable)

-- Prelude Data.Data> constrFields (toConstr (Testing undefined undefined))
-- ["greetings","testingthis"]

{-

deriveListField :: String -> a -> Context b
deriveListField listFieldName a = 
 let fields =  constrFields (toConstr a)
 in listField listFieldName (generateFields fields)
  where
    generateFields fields = foldr mappend missingField (map generateField fields)
    generateField f = field f (return . -- Stuck here. How do we cast the field string to, well, an actual field without using TH?
 
 -}

{- 
listField "facetList"
                                (
                                    field "facetName" (return . facetName . itemBody) <>
                                    field "facetList" (return . facetList . itemBody) <>
                                    field "facetPrettyName" (return . facetPrettyName . itemBody) <>
                                    field "facetPath" (return . facetPath . itemBody)
                                )
                                (sequence $ map (\(nm,p,t) -> makeItem $ generateFacetList nm p t) [("Tags","tags",tags),("Categories","categories",categories),("Libraries","libraries",libraries)]) <>
                   
                   -}
 
--instance HakyllField MyTestData where
--    toHakyllField = 
     
     

    
--instance HakyllField a => HakyllField [a] where
--    toHakyllField (x:xs) = listFieldWith -}