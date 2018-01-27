module Test where
import Debug.Trace
import Control.Monad

data Fruit = Apple | Orange | Mango

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd a b = case a of
  Nothing -> Nothing
  Just c -> case b of
    Nothing -> Nothing
    Just d -> Just (c + d)

-- | Doctest
--
-- >>> maybeAdd Nothing Nothing
-- Nothing
--
-- >>> maybeAdd (Just 0) (Just 5)
-- Just 5

fibonacci :: Int -> Integer
fibonacci x = head $ fibonacci' x
 where
  fibonacci' :: Int -> [Integer]
  fibonacci' x
   | x == 1 = [1]
   | x == 2 = [1,1]
   | x > 2 = (head z) + (head (tail z)):z
   | True = error"Must use an integer index greater than 0"
   where
    z = (fibonacci' (x - 1))

reverse' :: [a] -> [a]
reverse' a = reverse'' a []
 where
  reverse'' :: [a] -> [a] -> [a]
  reverse'' a b = case a of
   [] -> b
   x:xs -> reverse'' xs (x:b)

doubleLessThanFiveElseRemove :: [Int] -> [Int]
doubleLessThanFiveElseRemove a = [x * 2 | x <- a, x < 5]

mapMatrix :: [[a]] -> (a -> b) -> [[b]]
mapMatrix a f = map (map f) a

hello :: IO()
hello = putStr "Hello World"

getName :: IO()
getName = do
 putStr "What is your name?\n"
 name <- getLine
 putStr $ "Hello " ++ name ++ ".\n"

mimic :: IO()
mimic = do
 line <- getLine
 do action line where
  action line = case line of
   "" -> return ()
   _ -> do
    putStrLn line
    mimic

reverseWords :: String -> String
reverseWords sentence = unwords $ map reverse $ words sentence

loop :: IO()
loop = forever $ do
 putStr "Spam "

readUntil :: IO()
readUntil = do
 putStrLn "Give some input, ending it with |" --Doesn't work.
 data' <- getContents
 putStrLn $ "You entered: " ++ data'