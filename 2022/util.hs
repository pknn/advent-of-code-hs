module Util where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = foldr f [[]]
              where f c l@(x:xs) | c == d    = []:l
                                 | otherwise = (c:x):xs

solveIO :: (String -> a) -> FilePath -> IO a
solveIO s f = readFile f >>= (return . s)

groupN :: Int -> [a] -> [[a]]
groupN 0 xs = [xs]
groupN n [] = []
groupN n xs = [take n xs] ++ groupN n (drop n xs)

toInt :: String -> Int
toInt x = read x :: Int
