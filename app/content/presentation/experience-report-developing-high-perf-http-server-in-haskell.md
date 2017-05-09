---
title: "Experience Report: Developing High Performance HTTP/2 Server in Haskell"
url-video: "https://www.youtube.com/watch?v=YUiG0faZw8Y"
url-slides: "about:blank"
authors:
 - Kazuhiko Yamamoto
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-experience-report-developing-high-performance-http-2-server-in-haskell
tags:
 - Haskell In Production
 - Web Application
 - Benchmark
libraries:
 - warp
---

While the speed of the Internet has been increasing,
HTTP/1.1 has been plagued by
head-of-line blocking, low concurrency and redundant headers.
To solve these problems, HTTP/2 was standardized.

This paper summarizes our experience implementing HTTP/2 in Haskell.
We found several techniques to improve the performance of
the header compression
and identified a suitable data structure for HTTP/2 priority.

Also, we showed that Haskell lightweight threads
are useful for HTTP/2 where
the common tactics of one lightweight thread per connection cannot be used.

The HTTP/2 implementation of Warp,
the popular HTTP server library in Haskell,
ultimately provides better throughput than its HTTP/1.1 counterpart.
