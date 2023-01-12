import Util (splitOn, toInt)
import Data.List

input = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

toRange :: (Int,Int) -> [Int]
toRange (s,e) = [s..e]

groupInput :: String -> [([Int], [Int])]
groupInput = map splitComma . splitLine
                where
                    splitLine        = splitOn '\n'
                    splitComma       = toTuple2 . map splitDashToRange . splitOn ','
                    splitDashToRange = toRange . toTuple2 . map toInt . splitOn '-'
                    toTuple2 [x,y]   = (x,y)

fullyEnclosed :: [Int] -> [Int] -> Bool
fullyEnclosed xs ys = intersection == xs || intersection == ys
                        where
                            intersection = xs `intersect` ys

solveFirst = length . filter fullyEnclosed' . groupInput
                where 
                    fullyEnclosed' (f,s) = fullyEnclosed f s

solveSecond = length .filter isOverlapped . groupInput
                where
                    isOverlapped (f,s) = (intersect f s) /= []
