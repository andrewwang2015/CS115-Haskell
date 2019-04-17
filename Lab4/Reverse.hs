-- This program reads in all the lines of a file and prints them, in reverse
-- order, to standard output (terminal)

module Main where 

import System.Environment
import System.Exit
import Prelude

main :: IO()
main = do 
    lst_args <- getArgs  
    -- check to make sure we just receive filename
    case length lst_args of 
        -- If we receive the right amount of arguments (1)
        1 -> do 
            file_contents <- readFile (head lst_args)
            let lines_lst = lines file_contents
                reversed_lst = reverse lines_lst 
                in mapM_ (\s -> putStrLn s) reversed_lst >> exitSuccess
            -- Else, let's exitFailure with a message
        _ -> do 
            prog_name <- getProgName
            die ("Usage: " ++ prog_name ++ " filename")