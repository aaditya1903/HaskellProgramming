import Data.List
import qualified Data.Set as HS
import Test.QuickCheck

data Set a = Empty | Node (Set a) a (Set a)

toList :: Ord a => Set a -> [a]
toList Empty = []
toList (Node left value right) = toList left ++ [value] ++ toList right

fromList :: Ord a => [a] -> Set a
fromList = foldr Main.insert empty

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
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

singleton :: a -> Set a
singleton x = Node Empty x Empty

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Main.singleton x
insert x (Node left value right)
    | x < value = Node (Main.insert x left) value right
    | x > value = Node left value (Main.insert x right)
    | otherwise = Node left value right

union :: (Ord a) => Set a -> Set a -> Set a
union set1 Empty = set1
union Empty set2 = set2
union (Node left1 value1 right1) set2 = Main.insert value1 (Main.union (Main.union left1 right1) set2)

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Node left value right) set2
    | member value set2 = Node (intersection left set2) value (intersection right set2)
    | otherwise = Main.union (intersection left set2) (intersection right set2)

intersectionIdempotentProp :: IO ()
intersectionIdempotentProp =
    quickCheck
        ((\s -> HS.intersection s s == s) :: HS.Set Int -> Bool)

difference :: (Ord a) => Set a -> Set a -> Set a
difference set1 Empty = set1
difference Empty _ = Empty
difference (Node left value right) set2
    | member value set2 = Main.union (difference left set2) (difference right set2)
    | otherwise = Node (difference left set2) value (difference right set2)

member :: (Ord a) => a -> Set a -> Bool
member _ Empty = False
member x (Node left value right)
    | x < value = member x left
    | x > value = member x right
    | otherwise = True

cardinality :: Set a -> Int
cardinality Empty = 0
cardinality (Node left _ right) = 1 + cardinality left + cardinality right

setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ Empty = Empty
setmap f (Node left value right) = Node (setmap f left) (f value) (setmap f right)

setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr _ Empty acc = acc
setfoldr f (Node left value right) acc = setfoldr f left (f value (setfoldr f right acc))

findMax :: Set a -> a
findMax (Node _ value Empty) = value
findMax (Node _ _ right) = findMax right

removeSet :: (Eq a) => a -> Set a -> Set a
removeSet _ Empty = Empty
removeSet x (Node left value right)
    | x == val = 
        if Main.null (Node left _ _)
            then right
            else
                Node newLeft newVal right
    | otherwise = Node (removeSet x left) x (removeSet x right)
    where
        newVal :: a
        newVal = findMax (Node left _ _)

        newLeft :: Set a
        newLeft = removeSet newVal (Node left _ _)

main :: IO ()
main = do
    let set1 = foldr Main.insert Main.empty [1, 2, 3, 4, 5]
    let set2 = foldr Main.insert Main.empty [3, 4, 5, 6, 7]

    putStrLn "Set 1: "
    printSet set1

    putStrLn "\nSet 2: "
    printSet set2

    putStrLn "\nUnion of Set 1 and Set 2: "
    let unionSet = Main.union set1 set2
    printSet unionSet

    putStrLn "\nIntersection of Set 1 and Set 2: "
    let intersectionSet = Main.intersection set1 set2
    printSet intersectionSet

    putStrLn "\nDifference of Set 1 and Set 2: "
    let differenceSet = Main.difference set1 set2
    printSet differenceSet

    putStrLn "\nCardinality of Set 1: "
    print $ Main.cardinality set1

    putStrLn "\nMapped Set (doubling each element): "
    let mappedSet = Main.setmap (* 2) set1
    printSet mappedSet

    putStrLn "\nFolded Set (sum of elements): "
    let foldedResult = Main.setfoldr (+) set1 0
    print foldedResult

printSet :: (Show a) => Set a -> IO ()
printSet set = putStrLn $ "Set: " ++ showSet set

showSet :: (Show a) => Set a -> String
showSet Empty = "Empty"
showSet (Node left value right) = "Node (" ++ showSet left ++ ") " ++ show value ++ " (" ++ showSet right ++ ")"

