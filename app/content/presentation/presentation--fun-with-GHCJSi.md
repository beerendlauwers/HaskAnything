---
title: Fun with GHCJSi
url-video: https://www.youtube.com/watch?v=x7dQVZiWjvA
url-slides: about:blank
authors: Luite Stegeman
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags: 
- GHCJS
libraries: 
---

A recent new GHCJS feature is a REPL, GHCJSi. On the surface, GHCJSi looks exactly like GHCi. 
However, when a browser connects to the REPL session, all code is run inside the browser, giving the user full control over the browser’s HTML document and JavaScript environment. 

I will briefly discuss the design and implementation of GHCJSi and demonstrate how to use the REPL for browser-based Haskell development, making use of various GHCJS libraries.
