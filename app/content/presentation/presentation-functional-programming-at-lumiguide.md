---
title: Functional Programming at LumiGuide
url-video: https://www.youtube.com/watch?v=IKznN_TYjZk
url-slides: https://github.com/meiersi/HaskellerZ/raw/master/meetups/20160722-ZuriHac2016_Bas_van_Dijk_FP-at-LumiGuide/Bas_van_Dijk-FP_at_LumiGuide-ZuriHac2016.pptx
authors: Bas van Dijk
source: https://www.reddit.com/r/haskell/comments/51uaxw/zurihac_2016_functional_programming_at_lumiguide/
conference: ZuriHac 2016
tags: GHCJS,Machine Learning,Nix,Computer Vision
libraries: haskell-opencv,blaze-react,inline-c
---

I will give an introduction to LumiGuide and talk about how we use Haskell to build our bicycle detection and guidance systems. The first part of the talk will be non-technical but in the second part I will dive deep into haskell-opencv: a Haskell to OpenCV 3.1 binding that we recently open sourced. Roel van Dijk and I will do a live coding session showing how to bind a function from OpenCV to Haskell. Along the way we demonstrate the use of the inline-c library, describe how we handle C++ exceptions and show how we are using advanced type system features to encode more information into the type of matrices.
