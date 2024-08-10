import Data.Char

square :: Int -> Int
square x = x * x

pyth :: Int -> Int -> Int
pyth a b = square a + square b

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = pyth a b == square c

isTripleAny :: Int -> Int -> Int-> Bool
isTripleAny a b c = (pyth a b == square c) || (pyth b c == square a) || (pyth a c == square b)

halfEvens :: [Int] -> [Int]
halfEvens [] = []
halfEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b xs = [x | x <- xs, x >= a, x <= b]

capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]

capitaliseLong :: String -> String
capitaliseLong word = if (length word >= 4) then (capitalised word)
                       else (lowercase word)

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

title :: [String] -> [String]
title [] = []
title (w:words) = capitalised w : [capitaliseLong w | w <- words]


safeHeadPlusOne :: [Int] -> Int
safeHeadPlusOne xs = if null xs then 0 else head xs + 1

listMult :: [Int] -> Int
listMult [] = 0
listMult [x] = x
listMult (x:xs) = x * head xs

exOr :: Bool -> Bool -> Bool
exOr a b = (a || b) && not (a && b)

