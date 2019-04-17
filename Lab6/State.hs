import Control.Monad
import Control.Monad.State
import Data.IORef

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)

-- A.1
-- Computes factorials and works in the IO Monad
factIO :: Integer -> IO Integer
factIO a | a < 0 = error "Invalid input: less than 0."
         | otherwise = do
            total <- newIORef (1 :: Integer)
            counter <- newIORef (a :: Integer)
            whileIO
                (do counter1 <- readIORef counter 
                    return (counter1 /= 0))
                (do 
                    counter1 <- readIORef counter 
                    total1 <- readIORef total
                    writeIORef total (counter1 * total1)
                    writeIORef counter (counter1 - 1))
            readIORef total 

-- A.2
-- Computes factorials and uses a state monad 

factState :: Integer -> Integer 
factState a | a < 0 = error "Invalid input: less than 0"
            | otherwise = evalState helper (1, a) where 
                helper :: State (Integer, Integer) Integer 
                helper = 
                    do whileState (\(_, y) -> y /= 0)
                         (do (x, y) <- get 
                             put (x * y, y - 1))
                       (total,_) <- get 
                       return total

-- A.3
-- Computes fibonacci numbers using the IO Monad 

fibIO :: Integer -> IO Integer 
fibIO a | a < 0 = error "Invalid input: less than 0"
        | otherwise = do
            count <- newIORef (a :: Integer)
            first <- newIORef (0 :: Integer)
            next <- newIORef (1 :: Integer)
            whileIO 
                (do count1 <- readIORef count 
                    return (count1 /= 0))
                (do count1 <- readIORef count 
                    f <- readIORef first 
                    n <- readIORef next
                    writeIORef count (count1 - 1)
                    writeIORef first n 
                    writeIORef next (f + n))
            readIORef first 


-- A.4
-- Computes fibonacci numbers using the state monad
fibState :: Integer -> Integer 
fibState a | a < 0 = error "Invalid input: less than 0"
            | otherwise = evalState helper (0, 1, a) where 
                helper :: State (Integer, Integer, Integer) Integer 
                helper = 
                    do whileState (\(_, _, y) -> y /= 0)
                         (do (x, y, z) <- get 
                             put (y, x + y, z - 1))
                       (total,_, _) <- get 
                       return total

-- B.1
{-

Let's first derive the >>= operator for the Reader monad.

First, assume we have two functions in the Reader r monad with these
type signatures.

f :: a -> Reader r b
g :: b -> Reader r c 

and we would like to compose them to give a function with the type signature
h :: a -> Reader r c

Let's rewrite these type signatures in a non-monadic form so we can better
see what's going on

f' :: (a, r) -> b
g' :: (b, r) -> c 
h' :: (a,r) -> c

We can easily define h' in terms of f' and g':
h' (x, read) = 
    let y = f' (x, read)
        z = g' (y, read)
    in (z, read)

Going back to the original functions f, g, and h, we have:

h = f >=> g 

which is equivalent to:

h x = f x >>= g 

which is equivalent to (by reversing this equation):

f x >>= g = h x

Expanding h x, we have 
f x >>= g = Reader(\read -> h'(x, read))
          = Reader (\read ->
                let y = f' (x, read)
                    z = g' (y, read)
                in (z, read) )
          = Reader (\read -> 
                let y = f' (x, read) in g' (y, read))

Recall f x = Reader (\rd-> f'(x, Read))

fx >>= g = Reader(\read -> let (Reader ff) = f x 
                               y = ff read 
                            in g' (y, read))

Eliminating g' in favor of g

fx >>= g = Reader(\read -> let (Reader ff) = f x 
                               y = ff read 
                               (Reader gg) = g y 
                            in gg read)

Substituting mv for f x, g for ff, h for gg, x for y, r for read we get:

mv >>= g = Reader (\r -> 
            let (Reader g) = max
                x = g r
                (Reader h) = fx 
            in h r)

which is the definition provided in the assignment. 

Now, let's derive the return method.

The non-monadic state-passing functions have type signatures of the form:

(a, r) -> b

The identity function in this form would be :

id_reader (x, read) = x
id_reader' x read = x    -- Curried
id_reader' x = \read -> x   -- Written differently

Written as as function in the (State s) monad, this becomes
id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\read -> x)

This is exactly the definition provided in the assignment.

-}