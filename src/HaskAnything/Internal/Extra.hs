{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.Extra where

import           Hakyll

import           Data.Char                       (chr)
import           Data.ByteString.Lazy            (unpack,ByteString)

-- Lazy bytestring stuff
loadBodyLBS :: Identifier -> Compiler ByteString
loadBodyLBS = loadBody

-- |Convert a lazy ByteString to a string without double quotes screwing things up. ASCII only.
toString :: ByteString -> String
toString = map (chr . fromEnum) . unpack