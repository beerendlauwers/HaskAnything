{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}

module HaskAnything.Internal.Field where

import           Hakyll
import           Data.Data
import           Data.String.Utils             (replace)

urlReplaceField :: String -> (String,String) -> Context a
urlReplaceField fieldName (old,new) = field fieldName $ \item -> do
        mbFilePath <- getRoute (itemIdentifier item)
        case mbFilePath of
            Nothing       -> return "urlReplaceField: ???"
            Just filePath -> return $ toUrl $ replace old new $ filePath 
