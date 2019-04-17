module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- C.1

-- Instantiates a sparse matrix

sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix

-- Empty input list means empty datatypes
sparseMatrix [] b = SM b S.empty S.empty M.empty
sparseMatrix lst b@(r,c) 
    -- Check to make sure bounds are valid
    | r < 1 || c < 1 = error "Invalid bounds."
    -- Let's make sure all individual (row, col) pairs of list are in bounds
    | all (\((x, y), _)-> x <= r && y <= c) lst == False = error 
        "Element out of bounds."
    -- Now, we want only the nonzero elements to show up
    | otherwise = SM b (S.fromList non_z_rows) (S.fromList non_z_cols) 
        (M.fromList non_zero_tuples)
        where 
            -- Filter out the lst tuples that have a value of 0
            non_zero_tuples = filter (\(_, a) -> a /= 0) lst
            -- Get the rows of these non zero tuples
            non_z_rows = map (\((r,_), _) -> r) non_zero_tuples
            -- Get the columns of these non zero tuples
            non_z_cols = map (\((_,c), _) -> c) non_zero_tuples

-- C.2
-- Adds two compatible sparse matrices

addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM b1 _ _ v1) (SM b2 _ _ v2) 
    -- If bounds are different, then throw error
    | b1 /= b2 = error "Matrix dimensions are different."
    | otherwise = SM b1 rows cols vals 
        where 
            -- Add the corresponding maps together and filter out those 
            -- with values equal to 0
            vals = M.filter (/= 0) (M.unionWith (+) v1 v2) 
            -- Get the keys of this vals map to extract rows and columns
            keys = M.keys vals 
            rows = S.fromList (map (\(r, _) -> r) keys)
            cols = S.fromList (map (\(_, c) -> c) keys)

-- C.3
-- Negates a spare matrix 
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c v) = 
    let new_v = M.map (\x -> -x) v in SM b r c new_v 

-- C.4
-- Subtracts two compatible sparse matrices. We use negate and then add. 
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM m1 m2 = addSM m1 (negateSM m2)

-- C.5
-- Multiplies two compatible sparse matrices 
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM a@(SM (a1, b1) r1 _ _) b@(SM (a2, b2) _ c2 _) 
    | b1 /= a2 = error "Cannot multiply incompatible matrices"
    -- Use list comprehensions to multiply each nonzero row of v1 by 
    -- each nonzero column of v2 and feed in the resulting list into
    -- the constructor for sparseMatrix to create a new sparse matrix
    | otherwise = sparseMatrix 
    [((x,y), mulRowCol a b x y) | x <- S.toList r1, y <- S.toList c2]
    (a1, b2)


-- Multiplies specific row of first sparse matrix with specific 
-- column of second matrix 
mulRowCol :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a 
             -> Integer -> Integer -> a

mulRowCol (SM _ _ _ v1) (SM _ _ _ v2) r' c' = 
    -- Get the ((row, column), value) tuples in the row of first matrix 
    let proper_tuples = M.filterWithKey (\(r, _) _ -> r == r') v1
        -- Get the ((row, column), value) tuples in the col of second matrix 
        proper_tuples' = M.filterWithKey (\(_, c) _ -> c == c') v2 
        -- Now, we want to order the row elements by col. number
        -- and the column elements by row. number so we can then take 
        -- the intersection of these rows/cols which corresponds to 
        -- elements to multiply together  
        row_vals = M.mapKeys (\(_, c) -> c) proper_tuples
        col_vals = M.mapKeys (\(r, _) -> r) proper_tuples'
        -- Matching row indices/col indices multiplied together and then
        -- summed. 
        in sum(M.elems (M.intersectionWith (*) row_vals col_vals))

-- C.6
-- Three accessor functions

-- Retrieves a value from a sparse matrix given the row and column. Throws
-- error if indices are invalid/out of bounds. 

getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a

getSM (SM (r, c) _ _ v) (r', c') 
    | r' < 1 || c' < 1 || c' > c || r' > r = error "Out of bound indices"
    | otherwise = M.findWithDefault 0 (r', c') v

-- Returns the number of rows in a sparse matrix 
rowsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
rowsSM (SM (r, _) _ _ _) = r 

-- Returns the number of columns in a sparse matrix 
colsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
colsSM (SM (_, c) _ _ _) = c 

-- C.7 
-- Operator shortcut for addition
(<|+|>):: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

-- Operator shortcut for subtraction
(<|-|>):: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

-- Operator shortcut for multiplication
(<|*|>):: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

-- Operator shortcut for retrieving a specific value from matrix 
(<|!|>):: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<|!|>) = getSM

-- C.8 
{-
It does not make sense to define the SparseMatrix datatype as an instance of
the Num type class because it doesn't make that much sense to convert an
integer into matrix. Also, what would it mean to take the sign of a matrix?
Would it mean the sign of the determinant or something else? Because of 
these unclear situations, it doesn't make much sense to make SparseMatrix
and instance of the Num type class.
-}