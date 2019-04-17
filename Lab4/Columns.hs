-- Program prints the Nth column of each line of a file or Stdin

module Main where 

import System.Environment
import System.Exit
import Prelude
import Control.Monad()
import Data.Char
import Data.List
import Data.Maybe()
import System.IO


main :: IO()
main = 
    do 
        lst_args <- getArgs  
        if (length lst_args < 2 || 
            (length (filter isPositiveInt lst_args) /= length(lst_args) - 1)) 
            -- We need at least one column 
            then 
                die ("Usage: Columns n1 n2 ... filename (n1, n2, ... must " 
                ++ "be positive integers.)") 
            else 
                -- Get column numbers from the command line. 
                let cols = map (\s -> read s :: Int) (allButLast lst_args) 
                    in
                    if last lst_args == "-" 
                        then do
                            -- Handle standard input
                            content <- hGetContents stdin 
                            let allLines = lines content 
                                finalLst = map (\s -> returnColumnWords cols s) 
                                    allLines 
                                in putStrLn (intercalate "\n" finalLst)
                        else do 
                            -- Handle a file
                            content <- readFile (last lst_args)
                            let allLines = lines content 
                                finalLst = map (\s -> returnColumnWords cols s) 
                                    allLines 
                                in putStrLn (intercalate "\n" finalLst)


-- Returns true if the string is a positive integer. Used for checking valid
-- columns.

isPositiveInt :: String -> Bool
isPositiveInt [] = False 
isPositiveInt [x] = isDigit x
isPositiveInt (x : xs) = isDigit x && isPositiveInt xs

-- This function returns every element of a list except for the last 
allButLast :: [a] -> [a] 
allButLast [] = []
allButLast [_] = []
allButLast (x : xs) = x : allButLast xs


-- This function takes in a list of numbers and a string and returns 
-- a string with the words at these columns joined by a space 

returnColumnWords :: [Int] -> String -> String
returnColumnWords a str = 
    let words_lst = words str  
        -- Only get the valid columms by making sure we do not have
        -- any column numbers that surpass the number of words in our string
        valid_cols = filter (\x -> x <= length(words_lst)) a
        -- Get a list of words that correspond to each column. 
        valid_words = map (\col -> words_lst !! (col - 1)) valid_cols in
        -- Put these words together into single string with spaces in b/w
        intercalate " " valid_words