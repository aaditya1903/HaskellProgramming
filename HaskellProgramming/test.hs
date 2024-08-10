{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
    module Coursework where

    import Data.List
    import qualified Data.Set as HS (fromList, toList)
    import Test.QuickCheck
    
    {-
      Your task is to design a datatype that represents the mathematical concept of
      a (finite) set of elements (of the same type). We have provided you with an
      interface (do not change this!) but you will need to design the datatype and
      also support the required functions over sets. Any functions you write should
      maintain the following invariant: no duplication of set elements.
    
      There are lots of different ways to implement a set. The easiest is to use a
      list. Alternatively, one could use an algebraic data type, wrap a binary
      search tree, or even use a self-balancing binary search tree. Extra marks will
      be awarded for efficient implementations (a self-balancing tree will be more
      efficient than a linked list for example).
    
      You are **NOT** allowed to import anything from the standard library or other
      libraries. Your edit of this file should be completely self-contained.
    
      **DO NOT** change the type signatures of the functions below: if you do, we
      will not be able to test them and you will get 0% for that part. While sets
      are unordered collections, we have included the Ord constraint on some
      signatures: this is to make testing easier.
    
      You may write as many auxiliary functions as you need. Everything must be in
      this file.
    
      See the note **ON MARKING** at the end of the file.
    -}
    
    {-
       PART 1.
       You need to define a Set datatype.
    -}
    
    -- you **MUST** change this to your own data type. The declaration of Set a =
    -- Int is just to allow you to load the file into ghci without an error, it
    -- cannot be used to represent a set.
    data Set a = Set [a]
    
    {-
       PART 2.
       If you do nothing else, you must get the toList, fromList and equality working. If they
       do not work properly, it is impossible to test your other functions, and you
       will fail the coursework!
    -}
    
    insertSorted :: Ord a => a -> [a] -> [a]
    insertSorted x [] = [x]
    insertSorted x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insertSorted x ys
    
    insertionSort :: Ord a => [a] -> [a]
    insertionSort [] = []
    insertionSort (x:xs) = insert' x (insertionSort xs)
    
    -- toList {2,1,4,3} => [1,2,3,4]
    -- the output must be sorted.
    toList :: Ord a => Set a -> [a]
    toList (Set s) = foldr insertSorted [] s
    
    -- fromList: do not forget to remove duplicates!
    fromList :: Ord a => [a] -> Set a
    fromList xs = Set(insertionSort xs)
    
    -- Make sure you satisfy this property. If it fails, then all of the functions
    -- on Part 3 will also fail their tests
    toFromListProp :: IO ()
    toFromListProp =
      quickCheck
        ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)
    
    -- test if two sets have the same elements (pointwise equivalent).
    instance (Ord a) => Eq (Set a) where
      (Set s1) == (Set s2) = s1 == s2
    
    -- you should be able to satisfy this property quite easily
    eqProp :: IO ()
    eqProp =
      quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)
    
    {-
       PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
       TYPE SIGNATURES.
    -}
    
    -- the empty set
    empty :: Set a
    empty = Set []
    
    -- is it the empty set?
    null :: Set a -> Bool
    null empty = True
    
    -- build a one element Set
    singleton :: a -> Set a
    singleton x = Set [x]
    
    -- insert an element *x* of type *a* into Set *s* make sure there are no
    -- duplicates!
    insert :: (Ord a) => a -> Set a -> Set a
    insert x (Set s) = Set (insert' x s)
    
    insert' :: (Ord a) => a -> [a] -> [a]
    insert' y [] = [y]
    insert' y (z:zs)
      | y == z    = z:zs
      | y < z     = y:z:zs
      | otherwise = z : insert' y zs
    
    -- join two Sets together be careful not to introduce duplicates.
    union :: (Ord a) => Set a -> Set a -> Set a
    union (Set s1) (Set s2) = Set (insertionSort (s1 ++ s2))
    
    -- return, as a Set, the common elements between two Sets
    intersection :: (Ord a) => Set a -> Set a -> Set a
    intersection (Set s1) (Set s2) = Set (intersect s1 s2)
      where
        intersect :: (Ord a) => [a] -> [a] -> [a]
        intersect [] _ = []
        intersect _ [] = []
        intersect (x:xs) (y:ys)
          | x < y = intersect xs (y:ys)
          | x > y = intersect (x:xs) ys
          | otherwise = x : intersect xs ys
    
    -- all the elements in *s1* not in *s2*
    -- {1,2,3,4} `difference` {3,4} => {1,2}
    -- {} `difference` {0} => {}
    difference :: (Ord a) => Set a -> Set a -> Set a
    difference (Set s1) (Set s2) = Set (diff s1 s2)
      where
        diff :: (Ord a) => [a] -> [a] -> [a]
        diff [] _ = []
        diff xs [] = xs
        diff (x:xs) (y:ys)
          | x < y = x : diff xs (y:ys)
          | x > y = diff (x:xs) ys
          | otherwise = diff xs ys
    
    -- is element *x* in the Set s1?
    member :: (Ord a) => a -> Set a -> Bool
    member x (Set s1) = member' x s1
      where
      member' _ [] = False
      member' y (z:zs)
          | y == z    = True
          | otherwise = member' y zs
    
    -- how many elements are there in the Set?
    cardinality :: Set a -> Int
    cardinality (Set s) = length s
    
    -- apply a function to every element in the Set
    setmap :: (Ord b) => (a -> b) -> Set a -> Set b
    setmap f (Set s) = Set (map f s)
    
    -- right fold a Set using a function *f*
    setfoldr :: (a -> b -> b) -> Set a -> b -> b
    setfoldr f (Set s) acc = foldr f acc s
    
    -- remove an element *x* from the set
    -- return the set unaltered if *x* is not present
    removeSet :: (Eq a) => a -> Set a -> Set a
    removeSet x (Set s) = Set (filter (/= x) s)
    
    -- powerset of a set
    -- powerset {1,2} => { {}, {1}, {2}, {1,2} }
    powerSet :: Set a -> Set (Set a)
    powerSet (Set xs) = Set $ Set <$> powerSetList xs where
      powerSetList [] = [[]]
      powerSetList (y:ys) = powerSetList ys ++ map (y:) (powerSetList ys)
    
    -- TESTING:
    
    main :: IO ()
    main = do
      putStrLn "Testing toList, fromList, and toFromListProp:"
      toFromListProp
    
      putStrLn "\nTesting Eq instance with eqProp:"
      eqProp
    
      putStrLn "\nTesting Part 3 functions:"
      testEmpty
      testNull
      testSingleton
      testInsert
      testUnion
      testIntersection
      testDifference
      testMember
      testCardinality
      testSetMap
      testSetFoldr
      testRemoveSet
    
    testEmpty :: IO ()
    testEmpty = do
      putStrLn "empty:"
      print $ empty == Set ([] :: [Int])
    
    testNull :: IO ()
    testNull = do
      putStrLn "null:"
      print $ Coursework.null (Set []) == True
      print $ Coursework.null (Set [1, 2, 3]) == False
    
    testSingleton :: IO ()
    testSingleton = do
      putStrLn "singleton:"
      print $ Coursework.singleton 42 == Set [42]
    
    testInsert :: IO ()
    testInsert = do
      putStrLn "insert:"
      print $ Coursework.insert 42 (Set [1, 2, 3]) == Set [1, 2, 3, 42]
      print $ Coursework.insert 42 (Set [1, 2, 42]) == Set [1, 2, 42]
    
    testUnion :: IO ()
    testUnion = do
      putStrLn "union:"
      print $ Coursework.union (Set [1, 2, 3]) (Set [3, 4, 5]) == Set [1, 2, 3, 4, 5]
    
    testIntersection :: IO ()
    testIntersection = do
      putStrLn "intersection:"
      print $ intersection (Set [1, 2, 3]) (Set [3, 4, 5]) == Set [3]
    
    testDifference :: IO ()
    testDifference = do
      putStrLn "difference:"
      print $ difference (Set [1, 2, 3, 4]) (Set [3, 4]) == Set [1, 2]
    
    testMember :: IO ()
    testMember = do
      putStrLn "member:"
      print $ member 42 (Set [1, 2, 3, 42]) == True
      print $ member 42 (Set [1, 2, 3]) == False
    
    testCardinality :: IO ()
    testCardinality = do
      putStrLn "cardinality:"
      print $ cardinality (Set [1, 2, 3, 4]) == 4
    
    testSetMap :: IO ()
    testSetMap = do
      putStrLn "setmap:"
      print $ setmap (\x -> x * 2) (Set [1, 2, 3]) == Set [2, 4, 6]
    
    testSetFoldr :: IO ()
    testSetFoldr = do
      putStrLn "setfoldr:"
      print $ setfoldr (+) (Set [1, 2, 3]) 0 == 6
    
    testRemoveSet :: IO ()
    testRemoveSet = do
      putStrLn "removeSet:"
      print $ removeSet 42 (Set [1, 2, 3, 42, 4]) == Set [1, 2, 3, 4]
      print $ removeSet 42 (Set [1, 2, 3, 4]) == Set [1, 2, 3, 4]
    
    {-
       ON MARKING:
    
       Be careful! This coursework will be marked using QuickCheck, against
       Haskell's own Data.Set implementation. This testing will be conducted
       automatically via a marking script that tests for equivalence between your
       output and Data.Set's output. There is no room for discussion, a failing test
       means that your function does not work properly: you do not know better than
       QuickCheck and Data.Set! Even one failing test means 0 marks for that
       function. Changing the interface by renaming functions, deleting functions,
       or changing the type of a function will cause the script to fail to load in
       the test harness. This requires manual adjustment by a TA: each manual
       adjustment will lose 10% from your score. If you do not want to/cannot
       implement a function, leave it as it is in the file (with undefined).
    
       Marks will be lost for too much similarity to the Data.Set implementation.
    
       Pass: creating the Set type and implementing toList and fromList is enough
       for a passing mark of 40%, as long as both toList and fromList satisfy the
       toFromListProp function.
    
       The maximum mark for those who use Haskell lists to represent a Set is 70%.
       To achieve a higher grade than is, one must write a more efficient
       implementation. 100% is reserved for those brave few who write their own
       self-balancing binary tree.
    -}

powerSet :: Set a -> Set (Set a)
powerSet set = fromList' (map fromList' (generateSubsets (toList' set)))
  where
    generateSubsets :: [a] -> [[a]]
    generateSubsets [] = [[]]
    generateSubsets (x:xs) = subsets ++ map (x:) subsets
      where
        subsets = generateSubsets xs

toList' :: Set a -> [a]
toList' Empty = []
toList' (Node value left right) = toList' left ++ [value] ++ toList' right

fromList' :: [a] -> Set a
fromList' xs = foldr insert' Empty xs
  where
    insert' :: a -> Set a -> Set a
    insert' x set = Node x set Empty
