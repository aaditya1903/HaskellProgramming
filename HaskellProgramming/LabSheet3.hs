mult :: Num a => [a] -> a
mult = foldr (*) 1 

posList :: [Int] -> [Int]
posList = filter (\x -> x > 0)

trueList :: [Bool] -> Bool
trueList = foldr (&&) True

evenList :: [Int] -> Bool
evenList = foldr ((&&) . even) True

maxList :: Ord a => [a] -> a
maxList xs = foldr max (head xs) xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange n m xs = filter (\x->(x>= n) && (x<= m)) xs

countPositives :: [Int] -> Int
countPositives xs = foldr (+) 0 (map (\x->1) (filter (\x-> x>0) xs))

myLength :: [a] -> Int
myLength xs = foldr (+) 0 (map (\x -> 1) xs) 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) [ ]

myLength' :: [a] -> Int
myLength' xs =  foldr (+) 0 ( map' (\x-> 1) xs)
          where map' f = foldr ((:).f) [ ]

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