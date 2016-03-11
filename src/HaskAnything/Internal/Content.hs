{-# LANGUAGE OverloadedStrings #-}

module HaskAnything.Internal.Content where

import           Hakyll

matchContent :: String -> Context String -> Rules ()
matchContent name ctx = do
    let path = fromGlob ("content/" ++ name ++ "/*")
    match path $ do
        route $ setExtension ""
        let template = fromFilePath ("templates/content/" ++ name ++ ".html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate template ctx
            >>= loadAndApplyTemplate "templates/content.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls