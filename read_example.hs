import System.IO  
  
main = do  
    handle <- openFile "test.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  