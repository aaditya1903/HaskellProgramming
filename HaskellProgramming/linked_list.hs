import Data.List
import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck

data Set a = EmptySet | Node a (Set a)

toList :: Ord a => Set a -> [a]
toList set = setfoldr (:) set []

fromList :: Ord a => [a] -> Set a
fromList list = foldl (\acc x -> Main.insert x acc) empty list

toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

instance (Ord a) => Eq (Set a) where
    set1 == set2 = toList set1 == toList set2

eqProp :: IO ()
eqProp =
    quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

empty :: Set a
empty = EmptySet

null :: Set a -> Bool
null EmptySet = True
null _ = False

singleton :: a -> Set a
singleton x = Node x EmptySet

insert :: (Ord a) => a -> Set a -> Set a
insert x EmptySet = Node x EmptySet
insert x (Node y rest)
  | x < y = Node x (Node y rest)
  | x == y = Node y rest
  | otherwise = Node y (Main.insert x rest)

union :: (Ord a) => Set a -> Set a -> Set a
union set EmptySet = set
union EmptySet set = set
union (Node x rest1) set2 = Main.insert x (Main.union rest1 set2)

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection EmptySet _ = EmptySet
intersection _ EmptySet = EmptySet
intersection (Node x rest1) set2
  | member x set2 = Node x (intersection rest1 set2)
  | otherwise = intersection rest1 set2

difference :: (Ord a) => Set a -> Set a -> Set a
difference set EmptySet = set
difference EmptySet _ = EmptySet
difference (Node x rest1) set2
  | member x set2 = difference rest1 set2
  | otherwise = Node x (difference rest1 set2)

member :: (Ord a) => a -> Set a -> Bool
member _ EmptySet = False
member x (Node y rest)
  | x == y = True
  | otherwise = member x rest

cardinality :: Set a -> Int
cardinality EmptySet = 0
cardinality (Node _ rest) = 1 + cardinality rest

setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ EmptySet = EmptySet
setmap f (Node x rest) = Node (f x) (setmap f rest)

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr _ EmptySet acc = acc
setfoldr f (Node x rest) acc = f x (setfoldr f rest acc)


