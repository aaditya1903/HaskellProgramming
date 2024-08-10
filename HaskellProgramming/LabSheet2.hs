-- lab sheet 2
import Data.Char
import Data.List

-- inRangeRecursive
inRange :: Int -> Int -> [Int] -> [Int]
inRange n m [] = []
inRange n m (x : xs) | (n <= x) && (x <= m) = x : (inRange n m xs)
                     | otherwise = inRange n m xs

-- countPositivesRecursive
countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x : xs) | x > 0 = 1 + (countPositives xs)
                        | otherwise = countPositives xs

-- capitalisedRecursive
-- auxilaryFunction
auxCap :: [Char] -> [Char]
auxCap [] = []
auxCap (x : xs) = toLower x : auxCap xs

capitalised :: [Char] -> [Char]
capitalised [] = []
capitalised (x : xs) = (toUpper x) : (auxCap xs)

-- titleRecursive
-- auxilaryFunction
capLong :: [Char] -> [Char]
capLong [ ] = [ ]
capLong word | length word < 5 = word
             | otherwise = capitalised word

title :: [[Char]] -> [[Char]]
title [] = []
title (w : words) = (capitalised w) : ( titleAux words)
    where titleAux [ ] = [ ]
          titleAux (w : words) = (capLong w) : (titleAux words)

-- isort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)
   where insert' x [] = [x]
         insert' x (y : ys) = if x <= y then x : (y : ys) else y : (insert' x ys)

-- merge
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- rotor
rotor :: Int -> [Char] -> [Char]
rotor n [] = []
rotor 0 str = str
rotor n (c:str) | (0 <= n) && (n < length (c:str)) = rotor (n-1) (str ++ [c])
                | otherwise = error "offset out of range"

-- makeKey
makeKey :: Int -> [(Char,Char)]
makeKey n = zip upperAl (rotor n upperAl)
                where upperAl = ['A'..'Z']

-- lookUp

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = c
lookUp c ((c1,c2):pairs) | c == c1 = c2
                         | otherwise = lookUp c pairs

-- encipher
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

-- normalise
normalise :: String -> String
normalise [] = []
normalise (c:str) | (c `elem` ['A'..'Z']) || (c `elem` ['0'..'9']) = c:(normalise str)
                  | (c `elem` ['a'..'z']) = (toUpper c) : (normalise str)
                  | otherwise = normalise str


-- encipherStr
encipherStr :: Int -> String -> String
encipherStr n str = encipherStrAux n (normalise str)
   where encipherStrAux n [] = []
         encipherStrAux n (c:str) = (encipher n c) : (encipherStrAux n str)



-- tutorial qs on recursive function

elemNum :: Eq a => a -> [a] -> Integer
elemNum x xs | null xs = 0
             | x == head xs = 1 + (elemNum x (tail xs))
             | otherwise = elemNum x (tail xs)

unique' :: Eq a => [a] -> [a] -> [a] -> [a]
unique' xs [ ] acc = acc
unique' xs (y:ys) acc | elemNum y xs == 1 = unique' xs ys (y:acc)
                      | otherwise         = unique' xs ys acc

unique :: Eq a => [a] -> [a] 
unique xs = unique' xs xs [ ]

prop_elemNum_unique :: Eq a => a -> [a] -> Bool
prop_elemNum_unique x ys = ((elemNum x ys) == 1) || not (elem x (unique ys))

insert :: Ord a => a -> [a] -> [a]
insert x [ ] = [x]
insert x (y:ys) | x <= y    = x:(y:ys)
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [ ] = [ ]
isort (x:xs) = insert x (isort xs)

insert2 :: Ord a => a -> [a] -> [a]
insert2 x [ ] = [x]
insert2 x (y:ys) | x > y     = x:(y:ys)
                 | x == y    = x:ys
                 | otherwise = y:(insert2 x ys)

isort2 :: Ord a => [a] -> [a]
isort2 [ ] = [ ]
isort2 (x:xs) = insert2 x (isort2 xs)