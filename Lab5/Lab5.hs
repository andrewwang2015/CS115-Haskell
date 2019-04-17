import Control.Monad

-- A.1

hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do 
    i <- [1..]
    j <- [1..i-1]
    k <- [1..j-1]
    l <- [1..k-1]
    guard (i^3 + l^3 == j^3 + k^3)
    return ((i, l), (j, k), i^3 + l^3)

-- A.2

-- Using guard
sum_of_naturals :: Integer 
sum_of_naturals = sum $ do
    x <- [1..999]
    guard $ x `mod` 3 == 0 || x `mod` 5 == 0
    return x 

-- No guard, use mzero
sum_of_naturals2 :: Integer
sum_of_naturals2 = sum $ do
    x <- [1..999]
    if x `mod` 3 == 0 || x `mod` 5 == 0
        then return x 
        else mzero  

-- A.3

isPalindrome :: Integer -> Bool
isPalindrome a = show a == reverse (show a)

largestPalindrome :: Integer 
largestPalindrome = maximum $ do 
    i <- [100..999]
    j <- [100..999]
    guard (isPalindrome (i * j))
    return (i * j)

-- Answer: 906609

-- A.4

type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show


ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- List of all possible valid expressions: all possible combinations of the
-- digits 1 to 9 (in order) with one of the operators from the Op datatype
-- between each digit.

exprs :: [Expr]
exprs = do
    a <- ops
    b <- ops
    c <- ops
    d <- ops
    e <- ops
    f <- ops
    g <- ops
    h <- ops
    return [N 1, a, N 2, b, N 3, c, N 4, d, N 5, e, N 6, f, N 7, g, N 8, h, N 9]

-- Helper function that takes two integers and returns the two integers 
-- concatenated with one another.
concatInts :: Int -> Int -> Int
concatInts a b = read (show a ++ show b) :: Int 

-- Takes an expression and removes all instances of cat 
normalize :: Expr -> Expr 
normalize [] = []
normalize ((N i) : []) = [N i]
normalize ((N i) : (O Cat): (N j) : xs) = normalize(N(concatInts i j) : xs)
normalize ((N i) : (O x) : xs) = (N i) : (O x) : normalize (xs)
normalize _ = error "Invalid subexpression"

-- Takes in normalized expression and evaluates it to a given int
evaluate :: Expr -> Int 
evaluate [] = 0
evaluate ([N x]) = x
evaluate ((N x) : (O Add) : (N y) : xs) = evaluate (N (x + y) : xs)
evaluate ((N x) : (O Sub) : (N y) : xs) = evaluate (N (x - y) : xs)
evaluate _ = error "Invalid input expression to evaluate"

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs

-- B.1
{-
do n1 <- [1...6]
   n2 <- [1..6]
   []
   return (n1, n2)

Desugar do notation:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)

Replace >> with >>= \_->
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> return (n1, n2)

Substitute concatMap
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2)) [] 

Notice that we have concatMap _ [] so this evalutes to [].

-}

-- B.2
{-
Desugaring first expression:

do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)

Desugar do notation:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)

Replacing >> with >>= \ ->:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >>= \_ -> return (n1, n2)

Substitute concatMap:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> 
    (concatMap (\_ -> return (n1, n2)) return <anything>)

Susbtituting definition of concatMap:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> 
    concat (map \_ -> return (n1, n2) return <anything>)

Simplifying map expression:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> 
    concat (return (n1, n2))

Simplying concat: 
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)

Desugaring second expression:

do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)

Desugar do notation:

[1..6] >>= \n1 -> [1..6] >>= \n2 >> return (n1, n2)

Replacing >> with >>= \ ->:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)

which is the same as the first expression.
-}

-- B.3 

{-
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
    return [c1, c2]

Substitute for s:

do ['a', 'a', c1, c2, 'b', 'b'] <- 
    ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
        return [c1, c2]

Desugar do:

["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>= \n -> case n of
    ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
    _ -> fail "error"

Substituting >>= for concatMap:

concatMap (\n -> case n of 
            ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
            _ -> fail "error") 
            ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]

Evaluating concatMap:

concat (map (\n -> case n of
            ('a':'a':c1:c2:'b':'b') -> return [c1, c2]
            _ -> fail "error") 
            ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])

Evaluating concat:

concat (return [x,y], return [z,w], [], return [c,c], [])

Simplifying:

['xy', 'zw', 'cc']

If we used the default definition given in the Monad type class, we would
throw an error (and terminate) as soon as we saw "foobar", the first string that 
does not pattern match. 
-}

-- B.4

{-
foldr ((++) . k) [] m

for m = [x1, x2, ...]
foldr ((++) . k) [] [x1, x2, ...]
foldr (\x -> (++) (k x)) [] [x1, x2, ...]
[k x1, k x2, k x3, ...]

for m = []
foldr ((++) . k) [] []
[]

concat (map k m) 

for m = [x1, x2, ...]
concat (map k [x1, x2, ...])
concat ([k x1, k x2, ...])
[k x1, k x2, k x3 ,...]

for m= []
concat (map k [])
concat []
[]

So yes, they both compute the same thing.

-}

-- B.5
{-
The error occurs when it tries to do: AnyNum (n + s). This is because 
the addition operator (+) expects operands of the exact same type. The only
restriction that AnyNum imposes on n and s are that they are instances of the 
same type class Num. However, we are not guaranteed that n and s are precisely
the exact same instance. Thus, the compiler throws this error.

There is not a way to fix it without changing the AnyNum datatype. 
-}