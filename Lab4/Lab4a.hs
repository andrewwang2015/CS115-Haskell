module Lab4a where

-- A.1

myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) =
    putChar c >> myPutStrLn cs

-- A.2
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- A.3
{-
Simple method:

greet2 :: IO()
greet2 =
    putStr "Enter your name: " >> 
    getline >>= 
    \name -> (putStr "Hello, " >> putStr name >> putStrLn "!")

Complex method:

greet2: IO()
greet2 = 
    putStr "Enter your name: " >>
    getLine >>= 
        \y -> case y of
            name -> putStr "Hello, " >> putStr name >> putStrLn "!"
        _ -> fail "Pattern match failure in do expression" 

Both methods behave the same. This is due to the fact that there can never
be a pattern match failure in this case so the complex method will always
be able to match with the first case. 
-}

-- A.4
{-
Simple method:

greet3 :: IO()
greet3 = do
    putStr "Enter your name: " >>
    getLine >>= 
        \name -> let name = toUpper n : ns in
            putStr "Hello, " >> putStr name >> putStrLn "!"


Complex method:

greet3 :: IO()
greet3 = do 
    putStr "Enter your name: " >>
    getLine >>= 
        \y -> case name of 
            (n:ns) -> 
                let name = toUpper n : ns in 
                putStr "Hello, " >> putStr name >> putStrLn "!"
        _ -> fail "Pattern match failure in do expression"

-- Yes, in this case, our complex desugaring has effects. If the user
simply presses enter when prompted to enter his name, thus giving an empty
string answer, then our pattern match of (n : ns) expecting at least two 
elements will fail. In the second desugaring, we account for this while in the
first desugaring we do not.
-}