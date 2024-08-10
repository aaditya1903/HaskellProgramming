height' :: Set a -> Int
height' Empty = -1
height' (Node _ l r) = 1 + max (height' l) (height' r)

isBalanced' :: Set a -> Bool
isBalanced' Empty = True
isBalanced' (Node _ l r) = isBalanced' l && isBalanced' r && abs (height' l - height' r) < 2

leftElem' :: Set a -> Set a
leftElem' Empty = Empty
leftElem' (Node _ l _) = l

rightElem' :: Set a -> Set a
rightElem' Empty = Empty
rightElem' (Node _ _ r) = r

element' :: Set a -> a
element' (Node n _ _) = n

rotate' :: Set a -> Set a
rotate' Empty = Empty
rotate' (Node n l r)
  | not (isBalanced' l) = Node n (rotate' l) r
  | not (isBalanced' r) = Node n l (rotate' r)
  | height' l + 1 < height' r && height' (leftElem' r) < height' (rightElem' r) =
      Node (element' r) (Node n l (leftElem' r)) (rightElem' r)
  | height' r + 1 < height' l && height' (rightElem' l) < height' (leftElem' l) =
      Node (element' l) (leftElem' l) (Node n (rightElem' l) r)
  | height' l + 1 < height' r && height' (leftElem' r) > height' (rightElem' r) =
      Node (element' (leftElem' r)) (Node n l (leftElem' (leftElem' r))) (Node (element' r) (rightElem' (leftElem' r)) (rightElem' r))
  | height' r + 1 < height' l && height' (rightElem' l) > height' (leftElem' l) =
      Node (element' (rightElem' l)) (Node (element' l) (leftElem' l) (leftElem' (rightElem' l))) (Node n (rightElem' (rightElem' l)) r)
  | otherwise = Node n l r

insert' :: (a -> a -> Bool) -> a -> Set a -> Set a
insert' _ x Empty = Node x Empty Empty
insert' cmp x (Node n l r)
  | cmp x n   = rotate' (Node n (insert' cmp x l) r)
  | cmp n x   = rotate' (Node n l (insert' cmp x r))
  | otherwise = Node n l r

insert2 :: a -> Set a -> Set a
insert2 x Empty = Node x Empty Empty
insert2 x (Node n l r)
  | otherwise = Node n (insert2 x l) r

setmap' :: (a -> b) -> Set a -> Set b
setmap' _ Empty = Empty
setmap' f (Node n l r) = Node (f n) (setmap' f l) (setmap' f r)

union' :: Set a -> Set a -> Set a
union' Empty set2 = set2
union' set1 Empty = set1
union' (Node n1 l1 r1) set2 = Node n1 (union' (union' l1 r1) set2) Empty

union2 :: Set a -> Set a -> Set a
union2 Empty set2 = set2
union2 set1 Empty = set1
union2 (Node n1 l1 r1) set2 = insert2 n1 (union2 (union2 l1 r1) set2)

powerSet' :: Set a -> Set (Set a)
powerSet' Empty = Coursework.singleton empty
powerSet' (Node x l r) =
  let withoutX = powerSet' (union2 l r)
      withX = setmap' (insert2 x) withoutX
  in union2 withX withoutX