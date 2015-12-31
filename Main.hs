module Main where

import Control.Monad (forever, unless)
import Data.Char
import Data.List
import Data.Maybe
import System.Random

type Test = (String, String)

main = do
  text <- readFile "spell.txt"
  putStrLn "Press enter after every answer; twice to end the test."
  tests <- genTests $ genTestSet text
  loop tests

genTestSet :: String -> [(String, String)]
genTestSet text = map parseLine (lines text)
  where
    parseLine :: String -> Test
    parseLine str =
      let (word, clue) = break isSep str
      in  (word, dropWhile isSep clue)

isSep :: Char -> Bool
isSep = flip elem ",.;:\t"

genTests :: [(String, String)] -> IO [Test]
genTests testSet = do
  randomGen <- newStdGen
  let rands = compress $ randomRs (0, length testSet - 1) randomGen
  return $ map (testSet !!) rands

compress :: Eq a => [a] -> [a]
compress xs = map head $ group xs

loop :: [Test] -> IO ()
loop (tst : tsts) = do
  cont <- ask tst 2 -- give answer after n tries
  if not cont
  then do
    putStrLn "That was fun! See you next time."
    return ()
  else
    loop tsts


ask :: Test -> Int -> IO Bool
ask (word, clue) 0 = do
  putStrLn $ "Correct spelling is: " ++ word ++ "\n"
  return True
ask tst@(_, clue) n = do
  putStrLn clue
  str <- getLine
  if null str
  then return False
  else do
    yes <- check tst (strip str)
    if yes
    then return True
    else ask tst (n - 1)

strip str =
  takeWhile (not . isSep) $ dropWhile isSpace str


check :: Test -> String -> IO Bool
check (word, _) actual
    | word == actual =
      do putStrLn "** Correct! **\n"
         return True
    | actual == "?" =
      do putStrLn word
         return False
    | otherwise =
      do putStrLn "** Incorrect**"
         return False
