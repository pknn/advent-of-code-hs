import Data.Char
import Data.List
import Util (splitOn, solveIO, toInt)

groupInput :: String -> [[String]]
groupInput = splitOn "" . splitOn '\n'

toInt :: String -> Int
toInt x = read x :: Int

sumToInt :: [String] -> Int
sumToInt = sum . map toInt

solveFirst = maximum . map sumToInt . groupInput

solveSecond = sum . take 3 . reverse . sort . map sumToInt . groupInput
