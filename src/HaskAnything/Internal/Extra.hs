{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.Extra where

import           Hakyll

import           Data.Char                       (chr)
import           Data.ByteString.Lazy            (unpack,ByteString)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           System.FilePath                (takeBaseName, takeDirectory)

-- Lazy bytestring stuff
loadBodyLBS :: Identifier -> Compiler ByteString
loadBodyLBS = loadBody

-- |Convert a lazy ByteString to a string without double quotes screwing things up. ASCII only.
toString :: ByteString -> T.Text
toString = TL.toStrict . TL.decodeUtf8

-- Copied over from Hakyll.Web.Tags
getCategory :: MonadMetadata m => Identifier -> m [T.Text]
getCategory = return . return . T.pack . takeBaseName . takeDirectory . toFilePath

--
