--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List()
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1,1)
    where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter _ (10, _) = return True 
    iter s (r,c) = do
      cur <- readArray s (r,c)
      if cur == 0 then do 
        valid <- getOKValues s (r,c)
        iter' s (r,c) valid 
      else iter s (getNext (r,c))


    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' _ _ [] = return False 
    iter' s (r,c) (x:xs) = do 
      writeArray s (r,c) x 
      is_solved <- iter s (getNext(r, c)) 
      if (is_solved) then return True 
      else do 
        writeArray s (r, c) 0
        iter' s (r, c) xs 


    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues s (r,c) = do
      box_vals <- getBox s (r,c) 
      row_vals <- getRow s r 
      col_vals <- getCol s c
      return (filter (\val -> not (elem val box_vals || 
        elem val row_vals || elem val col_vals)) [1..9])

    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow s r = mapM (\c -> readArray s (r, c)) [1..9]

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol s c = mapM (\r -> readArray s (r, c)) [1..9]

    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox s (r,c) = 
      let rowBox = (r - 1) `div` 3 
          colBox = (c - 1) `div` 3 
          indices = [(i,j) | i <- [rowBox * 3 + 1 .. (rowBox + 1) * 3], 
                             j <- [colBox * 3 + 1 .. (colBox + 1) * 3] ]
          in mapM (\(i,j) -> readArray s (i,j)) indices

    -- Returns the next (row, column) location of Sudoku board to fill in
    getNext :: (Int, Int) -> (Int, Int)
    getNext (r, c) | c == 9 = (r+1, 1)
                   | otherwise = (r, c+1)


-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure