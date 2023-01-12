import Data.Char
import Util (splitOn, solveIO)

input :: String
input = "A Y\nB X\nC Z"

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred c
    | c == minBound = maxBound
    | otherwise     = pred c

  csucc :: a -> a
  csucc c
    | c == maxBound = minBound
    | otherwise = succ c

data Move = Rock | Paper | Scissors deriving (Eq, Enum, Bounded, Show)
instance CyclicEnum Move

data Result = Lose | Draw | Win deriving (Eq, Ord, Show)

toTuple2 :: [a] -> (a,a)
toTuple2 [x,y] = (x,y)

mapTuple2 :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]
mapTuple2 f g ts = map h ts
                     where h t = (f (fst t), g (snd t))

toMove :: String -> Move
toMove x
  | x == "A" || x == "X" = Rock
  | x == "B" || x == "Y" = Paper
  | x == "C" || x == "Z" = Scissors

toResult :: String -> Result
toResult x 
  | x == "X" = Lose
  | x == "Y" = Draw
  | x == "Z" = Win

toStrategicMove :: (Move,Result) -> (Move,Move)
toStrategicMove (m,r)
  | r == Draw = (m,m)
  | r == Win  = (csucc m, m)
  | r == Lose = (cpred m, m)

groupInput :: String -> [(String,String)]
groupInput = map toTuple2 . (map (splitOn ' ') . filter (/="") . splitOn '\n')

beat :: Move -> Move -> Result
beat x y
  | x == y       = Draw
  | csucc x == y = Lose
  | otherwise    = Win

beat' :: (Move,Move) -> Result
beat' t = fst t `beat` snd t

scoreResult :: Result -> Int
scoreResult Lose = 0
scoreResult Draw = 3
scoreResult Win  = 6

scoreMove :: Move -> Int
scoreMove = (+1) . fromEnum

totalScore :: (Move, Move) -> Int
totalScore t = (scoreResult . beat') t + scoreMove (fst t)

solveFirst :: String -> Int
solveFirst = sum . map totalScore . mapTuple2 toMove toMove . groupInput

solveSecond :: String -> Int
solveSecond = sum . map totalScore . map toStrategicMove . mapTuple2 toMove toResult . groupInput

solveFirstIO = solveIO solveFirst
solveSecondIO = solveIO solveSecond
