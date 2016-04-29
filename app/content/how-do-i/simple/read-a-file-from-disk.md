---
title: read a file from disk
---

Reading a file opens a handle to the file. You can use `openFile` to open a handle to the file and `hClose` to close the handle when you're done.
However, the `withFile` function closes the handle automatically for you, so it's a safer choice:

```Haskell
import System.IO     
    
main = do     
    withFile "file.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)  
``` 