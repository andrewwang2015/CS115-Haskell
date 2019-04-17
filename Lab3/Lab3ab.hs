module Lab3ab where 

-- A.1

{-
data Nat = Zero | Succ Nat 

instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ x) = "Succ (" ++ show x ++ ")" 

instance Eq Nat where 
    Zero        == Zero         = True
    Succ Zero   == Succ Zero    = True 
    Succ a      == Succ b       = a == b
    _           == _            = False 
-}

-- A.2
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- A.3
instance Ord Nat where
    Zero <= _ = True 
    _ <= Zero = False 
    Succ a <= Succ b = a <= b

{-
We could have had Haskell derive the Ord instance for us. By the way Nat
is defined, we know that Zero will be the least and that with each Succ,
will be the next step up. This makes sense in our case, as zero is indeed
the least element and with each additional Succ, our data gets larger.
-}

-- A.4

data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)

instance Eq SignedNat where 
    Neg Zero == Pos Zero = True
    Pos Zero == Neg Zero = True 
    Neg _ == Pos _ = False
    Pos _ == Neg _ = False 
    Neg a == Neg b = a == b
    Pos a == Pos b = a == b

instance Ord SignedNat where 
    Pos Zero <= Neg Zero = True 
    Neg _ <= Pos _ = True 
    Pos _ <= Neg _ = False 
    Neg a <= Neg b = a == b || a > b
    Pos a <= Pos b = a <= b

{- 
No, we cannot use automatically-derived definitions for the Eq and Ord
instances. Doing this would make Neg zero < Pos zero, which we know is false.
-}

-- A.5
absSignedNat:: SignedNat -> SignedNat 
absSignedNat (Pos a) = Pos a
absSignedNat (Neg a) = Pos a

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos Zero) = Pos Zero 
negateSignedNat (Neg a) = Pos a
negateSignedNat (Pos a) = Neg a

fromIntegerNat :: Integer -> Nat 
fromIntegerNat 0 = Zero 
fromIntegerNat a
    | a > 0 = Succ (fromIntegerNat(a - 1))
    | otherwise = Succ (fromIntegerNat(a + 1))

fromIntegerSignedNat :: Integer -> SignedNat 
fromIntegerSignedNat 0 = Pos Zero
fromIntegerSignedNat a | a > 0 = Pos (fromIntegerNat a)
                       | otherwise = Neg (fromIntegerNat a)

subNat:: Nat -> Nat -> Nat 
subNat a Zero = a 
subNat (Succ a) (Succ b) 
    | a == b = Zero 
    | otherwise = subNat a b
subNat _ _ = error "subNat: first arg. smaller than second."


addNat:: Nat -> Nat -> Nat 
addNat Zero a = a 
addNat a Zero = a
addNat (Succ a) (Succ b) = Succ (Succ (addNat a b))

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos a) (Pos b) = Pos (addNat a b)
addSignedNat (Neg a) (Neg b) = Neg (addNat a b)
addSignedNat (Pos a) (Neg b) | a == b = Pos Zero
                             | a > b = Pos (subNat a b)
                             | otherwise = Neg (subNat b a)
addSignedNat (Neg a) (Pos b) | a == b = Pos Zero
                             | a > b = Neg (subNat a b)
                             | otherwise = Pos (subNat b a)

sigNumSignedNat :: SignedNat -> SignedNat 
sigNumSignedNat (Pos a) | a == Zero = Pos Zero 
                        | otherwise = Pos (Succ Zero)
sigNumSignedNat (Neg a) | a == Zero = Pos Zero
                        | otherwise = Neg (Succ Zero)

subSignedNat :: SignedNat -> SignedNat -> SignedNat 
subSignedNat (Pos a) (Neg b) = Pos (addNat a b) 
subSignedNat (Neg a) (Pos b) = Neg (addNat a b) 
subSignedNat (Pos a) (Pos b) | a == b = Pos Zero 
                             | a < b = Neg (subNat b a)
                             | otherwise = Pos (subNat a b) 
subSignedNat (Neg a) (Neg b) | a == b = Pos Zero 
                             | a < b = Pos (subNat b a)
                             | otherwise = Neg (subNat a b)

mulNat :: Nat -> Nat -> Nat 
mulNat Zero _ = Zero 
mulNat _ Zero = Zero 
mulNat n@(Succ _) (Succ b) = addNat n (mulNat n b)

mulSignedNat :: SignedNat -> SignedNat -> SignedNat 
mulSignedNat (Neg Zero) _ = Pos Zero 
mulSignedNat _ (Neg Zero) = Pos Zero 
mulSignedNat (Pos Zero) _ = Pos Zero 
mulSignedNat _ (Pos Zero) = Pos Zero 
mulSignedNat (Pos a) (Pos b) = Pos (mulNat a b)
mulSignedNat (Pos a) (Neg b) = Neg (mulNat a b)
mulSignedNat (Neg a) (Pos b) = Neg (mulNat a b)
mulSignedNat (Neg a) (Neg b) = Pos (mulNat a b)

instance Num SignedNat where 
    negate = negateSignedNat
    (+) = addSignedNat 
    (*) = mulSignedNat 
    (-) = subSignedNat 
    fromInteger = fromIntegerSignedNat
    abs = absSignedNat
    signum = sigNumSignedNat

-- A.6
natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ a) = 1 + natToInteger a

signedNatToInteger :: SignedNat -> Integer 
signedNatToInteger (Pos a) | a == Zero = 0
                           | otherwise = natToInteger a 
signedNatToInteger (Neg a) | a == Zero = 0 
                           | otherwise = -natToInteger a

-- A.7 
{- 
Dealing with two versions of Zero, Pos Zero and Neg Zero, was annoying and 
misleading because in reality, they are the same thing. To get around this
issue, we can have a separate type for Zero and thus make the negatives 
and positives distinct. 


data UnaryInteger = Neg Unary | Zero | Pos Unary deriving (Show) 


Now, we have to define Unary, and because we no longer need to accomodate for
zero, we can have the "base" number be 1 instead of 0.

data Unary = One | Succ Unary deriving (Eq, Show, Ord)

Perhaps we have scaled down redundancy in code and improved time efficiency
(no need to check both Pos Zero and Neg Zero), but we still have pretty 
poor space inefficiency given that the amount of space and Succ's scales
linearly with the size of the number. To bring down this space complexity,
we can consider representing numbers in binay or hex, but then again that
gets pretty complicated with twos complements for representing negative
integers and all that juicy CS24 stuff. 
-}

-- A.8
factorial :: (Num a, Ord a) => a -> a 
factorial 0 = 1 
factorial n | n < 0 = error "negative arg. to factorial"
            | otherwise = n * factorial (n- 1)

-- result: Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- B.1
{-

>#< has to be infix. In the case where we have three integers, the result
of applying this operator will return a string and so we cannot chain the
result and use the >#< again which has a signature Integer -> Integer
-> String.

+|  is infixl, but can also be infixr. 2 +| (5 +| 11) = 2 +| 6 = 8 while
(2 +| 5) +| 11 = 7 +| 11 = 8

&< has to be infixl because it has a type signature [Integer] -> Integer 
-> [Integer] so we can have [1,2] &< 3 &< 4 and get [1,2,3] &< 4 and then
[1,2,3,4]. Obviously, we could not do [1,2] &< (3 &< 4) because the types
would fail.

<&& has to be infixr because it has type signature Integer -> [Integer] 
-> [Integer]. Let's take the example 5 <&& 1 <&& [2,3]. With infixr,
we would get 5 <&& [1,1,2,3] = [5,5,1,1,2,3]. If it was infixl, we would
have (5 <&& 1) <&& [2,3] and thus would be performing <&& on two integers
which would not type match.
-}

-- B.2

{-
For type checking, +# could be infixl, infixr, and infix. We can see this
by the type signature: Integer -> Integer -> Integer. We can see from this,
that as long as we simply have integers, chaining this operator will 
typecheck. 

However, to get desired behavior, we need it to be infix. 

Let's take 500000 +# 99 +# 1. Doing infixl, we get (900000 +# 99999) +# 1 = 
6 +# 1 = 1. Doing infixr, we get 900000 +# (99999 +# 1) = 900000 +# 6 = 6.
The desired behavior would be the number of digits in (900000 + 99999 + 1 = 
1000000) = 7. Thus, we need to restrict it to being infix.
-}