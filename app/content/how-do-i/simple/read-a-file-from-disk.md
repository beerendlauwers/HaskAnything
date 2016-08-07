---
title: read a file from disk
---

Use `readFile`:

```Haskell
main = do
    contents <- readFile "file.txt"
    putStr contents
```     
