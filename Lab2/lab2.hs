{-
Andrew Wang 
azwang
-}

module RedBlackTree where

-- A color is either red or black.
data Color = Red | Black
    deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
    deriving Show

{- A.1 -}
-- Returns true if specific value is in tree, false otherwise

member:: Ord a => a -> Tree a -> Bool
member _ Leaf = False 
member v (Node _ l v' r) | v == v' = True 
                         | v < v'= member v l 
                         | otherwise = member v r 

{- A.2 -}
-- Takes a red-black tree and returns list of tree elements given by 
-- in-order tree traversal.

toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ l v r) = toList l ++ [v] ++ toList r

{- A.3 -}
-- Insert new element into R-B tree

insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t) 
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right) 
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t = 
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black (Node Red l1 e1 (Node Red r1 e2 r2)) e t = 
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black l1 e1 (Node Red (Node Red r1 e2 r2) e t) =
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black l1 e1 (Node Red r1 e2 (Node Red r2 e t)) = 
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance color l e r = Node color l e r  -- no balancing needed

{- A.4 -}
-- Returns a red-black tree from inserting all elements of a given list

fromList:: Ord a => [a] -> Tree a 
fromList lst = foldr insert Leaf lst 

{- A.5 -}
-- Calculate max. and min. depths 

minDepth :: Tree a -> Int 
minDepth Leaf = 0 
minDepth (Node _ l _ r) = 1 + min (minDepth l) (minDepth r)

maxDepth :: Tree a -> Int 
maxDepth Leaf = 0 
maxDepth (Node _ l _ r) = 1 + max (maxDepth l) (maxDepth r)

{- A.6 -}
-- Tests first red-black tree invariant so that for any node, the 
-- node value is strictly larger than all node values in its left
-- subtree and strictly smaller than all node values in its right subtree

testInvariant1 :: Ord a => Tree a -> Bool 
testInvariant1 Leaf = True 
testInvariant1 (Node _ Leaf _ Leaf) = True 
testInvariant1 (Node _ l@(Node _ _ v1 _) v r@(Node _ _ v2 _))
    | v > v1 && v2 > v = testInvariant1 l && testInvariant1 r
testInvariant1 (Node _ Leaf _ r) = testInvariant1 r 
testInvariant1 (Node _ l _ Leaf) = testInvariant1 l 
testInvariant1 _ = False 

{- A.7 -}
-- Tests second red-black tree invariant, which is that no red node has a red
-- parent. Leaf nodes are considered black. 

testInvariant2 :: Tree a -> Bool 
testInvariant2 Leaf = True 
testInvariant2 (Node _ Leaf _ Leaf) = True 
testInvariant2 (Node Black l _ r) = testInvariant2 l && testInvariant2 r
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False 
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False 
testInvariant2 _ = True -- parent red and two children black is fine 

{- A.8 -}
-- Tests third red-black tree invariant, which is that all paths from the 
-- root down through a sequene of nodes to any leaf have the same number of 
-- black nodes. 

testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = 
        leafCounts left (n + 1) ++ leafCounts right (n + 1)
    leafCounts (Node Red left _ right) n = 
        leafCounts left n ++ leafCounts right n

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False

-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

{- B.1 -}
-- Takes two sets and returns true if first one is subset of other 

isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (`member` s2) (toList s1)

{- B.2 -}
-- Takes two sets as its arguments and returns True if the two sets are equal

eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = isSubset s1 s2 && isSubset s2 s1 

{- B.3 -}
-- Takes two sets and returns a new set which is the union of the input sets
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr (insert) s1 (toList s2)

{- B.4 -}
-- Takes two sets as its argument and returns a new set which is the 
-- intersection of the input sets. 
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = 
    foldr (\x r -> if member x s1 then insert x r else r) empty (toList s2)

{- B.5 -}
-- Takes two sets as its argument and returns a new set which is the 
-- set difference of the input sets.

difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = 
    foldr (\x r -> if member x s2 then r else insert x r) empty (toList s1)
