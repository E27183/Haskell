module ReadFromSystem where

import System.IO

main = do
 handle <- openFile "text.txt" ReadMode
 contents <- hGetContents handle
 putStrLn contents
 hClose handle --This part is important to close the file.

--or

main' = do
 withFile "text.txt" ReadMode (\handle -> do
  contents <- hGetContents handle
  putStrLn contents)

--copied equivalent
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

editFile = do
 putStrLn "Enter the new text you want to add"
 text <- getContents
 appendFile "text.txt" text

