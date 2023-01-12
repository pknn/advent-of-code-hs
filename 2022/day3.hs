import Util (splitOn, solveIO, groupN)
import Data.Maybe
import Data.List

input :: String
input = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

priorityList :: String
priorityList = ['a'..'z'] ++ ['A'..'Z']

priority :: Char -> Int
priority c = (+1) . fromJust $ elemIndex c priorityList

commonElem :: Eq a => [[a]] -> [a]
commonElem []        = []
commonElem (a:[])    = a
commonElem (a:a':as) = commonElem ([intersect a a'] ++ as)

solveFirst :: String -> Int
solveFirst = sum . map (priority . firstCommonChar) . groupInput
               where groupInput            = map splitHalf . splitOn '\n'
                     splitHalf s           = splitAt (halfLength s) s
                     halfLength s          = length s `div` 2
                     firstCommonChar (f,s) = (head . commonElem) [f,s]

solveSecond :: String -> Int
solveSecond = sum . map (priority . head . commonElem) . groupInput
                where groupInput = groupN 3 . splitOn '\n'

solveFirstIO  = solveIO solveFirst
solveSecondIO = solveIO solveSecond

