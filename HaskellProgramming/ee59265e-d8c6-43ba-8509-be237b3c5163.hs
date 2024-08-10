import Data.List
import qualified Data.Set as HS
import Test.QuickCheck

-- PART 1
data Set a = Empty | Node a (Set a) (Set a)

height :: Ord a => Set a -> Int
height Empty = -1
height (Node _ l r) = 1 + max (height l) (height r)

balanced :: Ord a => Set a -> Bool
balanced Empty = True
balanced (Node _ l r) = balanced l && balanced r && abs (height l - height r) <= 1

left :: Set a -> Set a
left Empty = Empty
left (Node _ l _) = l

right :: Set a -> Set a
right Empty = Empty
right (Node _ _ r) = r

value :: Ord a => Set a -> a
value Empty = error "Empty set has no value"
value (Node n _ _) = n

ins :: Ord a => Set a -> a -> Set a
ins Empty a = Node a Empty Empty
ins (Node b l r) k
  | b < k = rotate (Node b l (ins r k))
  | otherwise = rotate (Node b (ins l k) r)

rotate :: Ord a => Set a -> Set a
rotate Empty = Empty
rotate (Node n l r)
  | not (balanced l) = Node n (rotate l) r
  | not (balanced r) = Node n l (rotate r)
  | height l + 1 < height r && height (left r) < height (right r) =
      Node (value r) (Node n l (left r)) (right r)
  | height r + 1 < height l && height (right l) < height (left l) =
      Node (value l) (left l) (Node n (right l) r)
  | height l + 1 < height r && height (left r) > height (right r) =
      Node (value (left r)) (Node n l (left (left r))) (Node (value r) (right (left r)) (right r))
  | height r + 1 < height l && height (right l) > height (left l) =
      Node (value (right l)) (Node (value l) (left l) (left (right l))) (Node n (right (right l)) r)
  | otherwise = Node n l r

buildSet :: Ord a => [a] -> Set a
buildSet = foldl ins Empty
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
intersection set1 set2 = foldl (\acc x -> if Main.member x set1 then Main.insert x acc else acc) Main.empty (toList set2)

difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldl (\acc x -> if not (Main.member x s2) then Main.insert x acc else acc) empty (Main.toList s1)

