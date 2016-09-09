---
title: reddit-post-t1_d2hru95.md
author: torstengrust
unique_id: t1_d2hru95
subreddit: haskell
reddit-url: https://www.reddit.com/r/haskell/comments/4ght6b/%CE%BB_bubble_popa_simple_game_that_illustrates/d2hru95
tags: game
libraries:
---

Looks (and sounds!) great.

There's a caveat related to pattern matching, though.  If you try to pop/reduce

    take 3 [1,2,3,4,5]

and repeatedly pop the `take` bubble, the reduction goes astray.  You need to make sure to pop the `n - 1` bubble — for the then current value of `n` — at the right times to simulate the evaluation of pattern-matched arguments.

Still fun. \*pop\*
