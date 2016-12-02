{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.Extra where

import           Hakyll

import           Data.Char                       (chr)
import           Data.ByteString.Lazy            (unpack,ByteString)
import           System.FilePath                (takeBaseName, takeDirectory)

-- Lazy bytestring stuff
loadBodyLBS :: Identifier -> Compiler ByteString
loadBodyLBS = loadBody

-- |Convert a lazy ByteString to a string without double quotes screwing things up. ASCII only.
toString :: ByteString -> String
toString = map (chr . fromEnum) . unpack

-- Copied over from Hakyll.Web.Tags
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath
