-- lab sheet 1 questions
import Data.Char
import Data.List

-- square
square :: Int -> Int
square a = a*a

--pyth
pyth :: Int -> Int -> Int
pyth a b = square a + square b

--isTriple
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = pyth a b == square c

--isTripleAny
isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c = pyth a b == square c || pyth b c == square a || pyth a c == square b

--halfEvens
halfEvens :: [Int] -> [Int]
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <-xs]

--inRange
inRange :: Int -> Int -> [Int] -> [Int]
inRange lowlim uplim xs = [x | x <- xs, lowlim <= x, uplim >= x]

--countPositives
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

--capitalised
capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]

-- title
-- auxiliary functions 
capitaliseLong :: String -> String
capitaliseLong word = if (length word >= 4) then (capitalised word) else (lowercase word)

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

title :: [String] -> [String]
title [] = []
title (w:words) = capitalised w : [capitaliseLong w | w <- words]

-- large group tutorial questions

plusone :: [Int] -> Int
plusone [] = 0
plusone (x:xs) = x+1

adding :: [Int] -> Int
adding [] = 0
adding (x:[]) = x
adding (x:y:xs) = x + y

plusone' :: [Int] -> Int
plusone' xs = if null xs then 0 else (head xs) + 1

adding' :: [Int] -> Int
adding' xs = if null xs then 0 else if null (tail xs) then head xs else head xs + head (tail xs)

firstDigit' :: String -> String
firstDigit' [] = []
firstDigit' (x : xs)  = if (elem x ['0' .. '9']) then [x] else firstDigit'  xs

exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr False True = True
exOr True False = True
exOr True True = False

exOr2 :: Bool -> Bool -> Bool
exOr2 a b = not (a == b)