-- B.1
(+*) :: Double -> Double -> Double
x +* y = x^2 + y^2
infixl 7 +*

(^||) :: Bool-> Bool -> Bool
False ^|| x = x
True ^|| x = not x
infixr 3 ^||

-- B.2

rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y = error "Invalid inputs. First arg > second arg."
                 | x == y = y
                 | otherwise = x * rangeProduct (x+1) y

-- B.3

prod :: [Integer] -> Integer
prod = foldr (*) 1

rangeProduct2:: Integer -> Integer -> Integer
rangeProduct2 x y | x > y = error "Invalid inputs. First arg > second arg."
                  | otherwise = prod [x..y]

-- B.4

map2:: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = (f x y) : map2 f xs ys
map2 _ _ _ = []

map3:: (a -> b ->c -> d) -> [a] -> [b] -> [c] -> [d]
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : map3 f xs ys zs
map3 _ _ _ _ = []


{-
dot = (sum .) . map2 (*)
    = (sum .) .map2 (*) lst1 lst2
    = (\x -> sum . x) . map2 (*) lst1 lst2
    = (\x -> sum. x) (map2 (*) lst1) lst2
    = (sum. (map2 (*) lst1)) lst2
    = (sum (map2 (*) lst1 lst2))
    = sum (map2 (*) lst1 lst2)
-}

-- B.5

sum_of_naturals = sum [x | x <- [0..999], x `mod` 3 ==0 || x `mod` 5 == 0]
-- result: 233168

-- B.6

sieve:: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\a -> a `mod` x /= 0) xs)

primes = sieve[2..]

sumPrimes = sum(takeWhile (<10000) primes)
-- sumPrimes = 5736396

-- C.1

{- Instead of using "head" and "tail" to reference parts of a list, we should
use direct pattern matching. This helps us avoid unecessary function calls. -}
sumList:: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- C.2

{- Similar to the previous problem, we should use pattern matching to avoid
unneccesary function calls to head/tail. Similarly, we do not need to use
the length function; we can simply use pattern matching to match the empty
list and list with single element. 
-}
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)

-- D.1

{-
fib 3
fib (3-1) + fib (3-2)
fib (2) + fib(3-2)
(fib(2-1) + fib(2-2) + fib(3-2)
(fib 1 + fib(2-2)) + fib(3-2)
(1 + fib(2-2)) + fib(3-2)
(1 + fib(0)) + fib(3-2)
(1 + 0) + fib(3-2)
1 + fib(3-2)
1 + fib(1)
1 + 1
2
-}

-- D.2

{-
fact 3
3 * fact(3-1)
3 * ((3-1) * fact((3-1)-1))
3 * (2 * fact((3-1)-1))
3 * (2 * ((3-1)-1) * fact(((3-1)-1)-1))
3 * (2 * ((2-1) * fact(((3-1)-1)-1))
3 * (2 * 1 * fact(((3-1)-1)-1))
etc.... (does not terminate)


The problem lies in the order of pattern matching. The way it is currently
written, Haskell will match the argument of fact against "n" and not 0 because
"fact n" comes before "fact 0." Thus, the base case is never reached, and so
our program does not terminate. 
-}

-- D.3

{-
reverse [1,2,3]
iter [1,2,3] []
iter [2,3] (1:[])
iter [3] (2:1:[]))
iter [] (3:(2:(1:[])))
(3:(2:(1:[])))
3 : (2 : [1])
3: [2,1]
[3,2,1]

The time complexity is O(N) where N is the length of the input list. Each
iteration moves the head of the input list to the reversed list, which takes
constant time. Because there are N elements to move, we have an asymptotic
time complexity of O(N).
-}

-- D.4

{-
reverse [1,2,3]
reverse [2,3] ++ [1]
(reverse [3] ++ [2]) ++ [1]
((reverse [] ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
([3] ++ [2]) ++ [1]
(3: ([] ++ [2])) ++ [1]
(3: [2]) ++ [1]
[3,2] ++ [1]
3 : ([2] ++ [1])
3 : (2 : ([] ++ [1]))
3: (2 : [1])
3 : [2,1]
[3,2,1]

The time complexity here is O(N^2). After deconstructing reverse into an
expression of lists operated on by the "++" operator which takes linear time,
we see that to actually construct the reversed list takes O(N^2) time. Unlike
the previous problem where we were using the cons operator, in this case, 
we are using the list concatenation operator which takes O(n) time where n is
the length of the first argument. In total, we have O(N) concatenations of
lists that could be up to size (N-1), and so in total, we have an asymptotic
time complexity of O(N^2). 
-}

-- D.5

{-
head (isort [3,1,2,5,4])
head (insert 3 (isort [1,2,5,4]))
head (insert 3 (insert 1 (isort[2,5,4])))
head (insert 3 (insert 1 (insert 2 (isort [5,4]))))
head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
head (insert 3 (insert 1 (2 : 4 : insert 5 []))))
head (insert 3 :  1 (2 : 4 : insert 5 []))))
head (1 : insert 3 (2 : 4 : insert 5 []))))
1
-}

-- D.6

{-
foldr max 0 [1,5,3,-2,4]
max 1 (foldr max 0 [5,3,-2,4])
max 1 (max 5 (foldr max 0 [3, -2, 4]))
max 1 (max 5 (max 3 (foldr 0 [-2,4])))
max 1 (max 5 (max 3 (max -2 (foldr 0 [4]))))
max 1 (max 5 (max 3 (max -2 (max 4 (foldr 0 [])))))
max 1 (max 5 (max 3 (max -2 (max 4 0))))
max 1 (max 5 (max 3 (max -2 4)))
max 1 (max 5 (max 3 4 ))
max 1 (max 5 4)
max 1 5
5


foldl max 0 [1,5,3,-2,4]
foldl max (max (0 1)) [5,3,-2,4]
foldl max (max (max 0 1) 5) [3,-2,4]
foldl max (max (max (max 0 1) 5) 3) [-2,4]
foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
(max (max (max (max (max 0 1) 5) 3) -2) 4) 
(max (max (max (max 1 5) 3) -2) 4) 
(max (max (max 5 3) -2) 4) 
(max (max 5 -2) 4)
(max 5 4)
5

The space complexities of foldr and foldl are the same. Due to Haskell's 
lazy evaluation, both foldr and foldl must fully expand itself and
evaluate before simplification. 
-}