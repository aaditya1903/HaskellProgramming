import Data.List
import qualified Data.Set as HS
import Test.QuickCheck

-- PART 1
data Set a = Empty | Node a (Set a) (Set a)

instance (Ord a, Show a) => Show (Set a) where
  show :: (Ord a, Show a) => Set a -> String
  show = show . toList

instance Ord a => Ord (Set a) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Node x l1 r1) (Node y l2 r2) =
    case compare x y of
      EQ -> compare l1 l2 <> compare r1 r2
      other -> other

height :: Ord a => Set a -> Int
height Empty = -1
height (Node _ l r) = 1 + max (height l) (height r)

isBalanced :: Ord a => Set a -> Bool
isBalanced Empty = True
isBalanced (Node _ l r) = isBalanced l && isBalanced r && abs (height l - height r) < 2

leftElem :: Set a -> Set a
leftElem Empty = Empty
leftElem (Node _ l _) = l

rightElem :: Set a -> Set a
rightElem Empty = Empty
rightElem (Node _ _ r) = r

element :: Ord a => Set a -> a
element (Node n _ _) = n

rotate :: Ord a => Set a -> Set a
rotate Empty = Empty
rotate (Node n l r)
  | not (isBalanced l) = Node n (rotate l) r
  | not (isBalanced r) = Node n l (rotate r)
  | height l + 1 < height r && height (leftElem r) < height (rightElem r) =
      Node (element r) (Node n l (leftElem r)) (rightElem r)
  | height r + 1 < height l && height (rightElem l) < height (leftElem l) =
      Node (element l) (leftElem l) (Node n (rightElem l) r)
  | height l + 1 < height r && height (leftElem r) > height (rightElem r) =
      Node (element (leftElem r)) (Node n l (leftElem (leftElem r))) (Node (element r) (rightElem (leftElem r)) (rightElem r))
  | height r + 1 < height l && height (rightElem l) > height (leftElem l) =
      Node (element (rightElem l)) (Node (element l) (leftElem l) (leftElem (rightElem l))) (Node n (rightElem (rightElem l)) r)
  | otherwise = Node n l r

-- PART 1

-- PART 2
toList :: Ord a => Set a -> [a]
toList set = inOrder set []
  where
    inOrder Empty acc = acc
    inOrder (Node n l r) acc = inOrder l (n : inOrder r acc)

fromList :: Ord a => [a] -> Set a
fromList = foldl (\acc x -> Main.insert x acc) Empty

toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

instance (Ord a) => Eq (Set a) where
    set1 == set2 = toList set1 == toList set2

eqProp :: IO ()
eqProp =
    quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)
-- PART 2

-- PART 3
empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

singleton :: a -> Set a
singleton x = Node x Empty Empty
    
insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node n l r)
  | x < n     = rotate (Node n (Main.insert x l) r)
  | x > n     = rotate (Node n l (Main.insert x r))
  | otherwise = Node n l r

union :: Ord a => Set a -> Set a -> Set a
union set1 set2 = foldl (\acc x -> Main.insert x acc) set1 (Main.toList set2)

member :: Ord a => a -> Set a -> Bool
member _ Empty = False
member x (Node n l r)
  | x < n     = Main.member x l
  | x > n     = Main.member x r
  | otherwise = True

intersection :: Ord a => Set a -> Set a -> Set a
intersection set1 set2 = foldl (\acc x -> if Main.member x set1 then Main.insert x acc else acc) Main.empty (Main.toList set2)

difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldl (\acc x -> if not (Main.member x s2) then Main.insert x acc else acc) empty (Main.toList s1)

cardinality :: Set a -> Int
cardinality Empty = 0
cardinality (Node _ l r) = 1 + cardinality l + cardinality r

setmap :: Ord b => (a -> b) -> Set a -> Set b
setmap _ Empty = Empty
setmap f (Node n l r) = Node (f n) (setmap f l) (setmap f r)

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr _ Empty acc = acc
setfoldr f (Node n l r) acc = setfoldr f l (f n (setfoldr f r acc))

removeSet :: Ord a => a -> Set a -> Set a
removeSet _ Empty = Empty
removeSet x (Node value left right)
    | x < value = Node value (removeSet x left) right
    | x > value = Node value left (removeSet x right)
    | otherwise = mergeSets left right
  where
    mergeSets :: Ord a => Set a -> Set a -> Set a
    mergeSets Empty right = right
    mergeSets left Empty = left
    mergeSets left right = Node minValue left (removeSet minValue right)
      where
        minValue = findMinValue right
        findMinValue (Node value Empty _) = value
        findMinValue (Node _ left _) = findMinValue left

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet Empty = Main.singleton empty
powerSet (Node x l r) =
  let withoutX = powerSet (Main.union l r)
      withX = setmap (Main.insert x) withoutX
  in Main.union withoutX withX


removeSet' :: Ord a => a -> Set a -> Set a
removeSet' _ Empty = Empty
removeSet' x (Node value left right)
    | x < value = balance (Node value (removeSet' x left) right)
    | x > value = balance (Node value left (removeSet' x right))
    | otherwise = mergeSets left right
  where
    mergeSets :: Ord a => Set a -> Set a -> Set a
    mergeSets Empty right = right
    mergeSets left Empty = left
    mergeSets left right = balance (Node minValue left (removeSet' minValue right))
      where
        minValue = findMinValue right
        findMinValue (Node value Empty _) = value
        findMinValue (Node _ left _) = findMinValue left

    balance :: Ord a => Set a -> Set a
    balance Empty = Empty
    balance (Node n l r)
        | height l - height r > 1 = rotateRight n l r
        | height r - height l > 1 = rotateLeft n l r
        | otherwise = Node n l r

    rotateRight :: Ord a => a -> Set a -> Set a -> Set a
    rotateRight n (Node ln ll lr) r = Node ln ll (Node n lr r)

    rotateLeft :: Ord a => a -> Set a -> Set a -> Set a
    rotateLeft n l (Node rn rl rr) = Node rn (Node n l rl) rr

    -- Eq FUNCTIONS
unioneq :: (Eq a) => Set a -> Set a -> Set a
unioneq Empty set2 = set2
unioneq set1 Empty = set1
unioneq (Node n1 l1 r1) set2 = inserteq n1 (unioneq (unioneq l1 r1) set2)

inserteq :: (Eq a) => a -> Set a -> Set a
inserteq x Empty = Node x Empty Empty
inserteq x (Node n l r)
  | x == n    = Node n l r
  | otherwise = Node n (inserteq x l) r
-- Eq FUNCTIONS

-- PART 3