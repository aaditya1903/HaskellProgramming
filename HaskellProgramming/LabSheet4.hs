-- question 1.
sqList :: [Integer] -> [Integer]
sqList  = map (\x -> x*x) 

sumSqList :: [Integer] -> Integer
sumSqList = foldr (+) 0 . sqList

posList :: [Integer] -> Bool
posList = foldr (&&) True . map (>0)

-- question 2.

fMin :: Ord a => (Integer -> a) -> Integer -> a
fMin f n = minimum $ map f [0..n]

eqF :: Eq a => (Integer -> a) -> Integer -> Bool
eqF f n = foldr (&&) True $  map ((== f(0)) . f) [0..n]

posF :: (Num a, Ord a) => (Integer -> a) -> Integer -> Bool
posF f n = foldr (&&) True $ map ((> 0) . f) [0..n]

incOrd :: Ord a => [a] -> Bool
incOrd xs = foldr (&&) True $ map (\(x,y) -> x <= y) $ zip xs (tail xs)

-- question 3.

twice :: (a -> a) -> a -> a
twice f x = f $ f x

-- twice f = f . f {- pointless -}

-- question 4.

iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = iter (n-1) f $ f x 

-- iterAcc :: Integer -> (a -> a) -> (a -> a) -> a -> a
-- iterAcc 0 f g = g
-- iterAcc n f g = iterAcc (n-1) f (f.g)
-- iter n f = iterAcc n f id  {- pointless -}

-- question 5.

double :: Integer -> Integer
double n = 2*n

-- double = (*2) {- pointless-}



powerOfTwo :: Integer -> Integer
powerOfTwo 0 = 1
powerOfTwo n = iter (n-1) double 2



-- question 6.


data RhType = Pos | Neg


data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType



patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos


showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh


canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True                      
canDonateTo _ (BloodType AB _) = True                     
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise


import Data.Char
import Data.List
import Test.QuickCheck

-- halveEvens
halveEvens :: [Int] -> [Int]
halveEvens xs = [if x `mod` 2 == 0 then x `div` 2 else x | x <- xs]

halveEvRec :: [Int] -> [Int]
halveEvRec [ ] = [ ]
halveEvRec (x : xs) | x `mod` 2 == 0 = (x `div` 2) : (halveEvRec xs)
                    | otherwise = x : (halveEvRec xs)

-- prop_halveEv
prop_halveEv :: [Int] -> Bool
prop_halveEv xs =  halveEvens xs == halveEvRec xs           

-- inRange
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

-- prop_inRange
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = res >= take len (cycle [lo]) && res <= take len (cycle [hi])
                        where res = inRange lo hi xs
                              len = length res

-- countPositives
countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

-- countPosHO
countPosHO :: [Int] -> Int
countPosHO xs  = length $ filter (> 0) xs

-- prop_countPos
prop_countPos :: [Int] -> Bool
prop_countPos xs = countPositives xs == countPosHO xs

-- capitalised
capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]

-- prop_capital

prop_capital :: String -> Bool
prop_capital xs = capitalised (capitalised xs) == capitalised xs

-- rotor
rotor :: Int -> [Char] -> [Char]
rotor n [] = []
rotor 0 str = str
rotor n (c:str) | (0 <= n) && (n < length (c:str)) = rotor (n-1) (str ++ [c])
 --               | otherwise = error "offset out of range"

-- prop_rotor
prop_rotor :: Int -> String -> Property
prop_rotor k str =  
    k < (length str) && 0 <= k ==> 
            take k str == drop (n-k) result && ( drop k str == take (n-k) result )
                      where result = rotor k str
                            n = length str

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

-- prop_encipher
prop_encipher :: Int -> Char -> Bool
prop_encipher n c = 
     encipher n c == encipher 13 (encipher (n + 13) c) 

-- normalise
normalise :: String -> String
normalise [] = []
normalise (c:str) | (c `elem` ['A'..'Z']) || (c `elem` ['0'..'9']) = c:(normalise str)
                  | (c `elem` ['a'..'z']) = (toUpper c) : (normalise str)
                  | otherwise = normalise str

-- prop_norm
prop_norm :: String -> Bool
prop_norm str =
    normalise str == normalise (normalise str)

-- words, unwords: see definition in Prelude

-- prop_words
prop_words :: String -> Bool
prop_words str = unwords . words str == str -- note that using ground types allows meaningful counter examples
