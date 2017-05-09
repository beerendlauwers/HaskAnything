---
title: "QuickFuzz: An Automatic Random Fuzzer for Common File Formats"
url-video: "https://www.youtube.com/watch?v=s7h4dhgaPCU"
url-slides: "about:blank"
authors:
 - Gustavo Grieco
 - Martín Ceresa
 - Pablo Buiras
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-quickfuzz-an-automatic-random-fuzzer-for-common-file-formats
tags:
 - Fuzzing
 - Testing
libraries:
 - template-haskell
---


Fuzzing is a technique that involves testing programs using invalid
or erroneous inputs. Most fuzzers require a set of valid inputs as a
starting point, in which mutations are then introduced. QuickFuzz is a
fuzzer that leverages QuickCheck-style random test-case generationto automatically test programs that manipulate common file formats
by fuzzing. We rely on existing Haskell implementations of
file-format-handling libraries found on Hackage, the
community-driven Haskell code repository. We have tried QuickFuzz
in the wild and found that the approach is effective in
discovering vulnerabilities in real-world implementations of browsers,
image processing utilities and file compressors among others.
In addition, we introduce a mechanism to automatically derive random generators for the types
representing these formats. QuickFuzz handles most well-known image
and media formats, and can be used to test programs and libraries
written in any language.
