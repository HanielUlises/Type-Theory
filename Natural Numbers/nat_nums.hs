-- Church Encoding of Natural Numbers in Lambda Calculus

type Nat = (a -> a) -> a -> a

-- Church Numerals
zero :: Nat
zero = \f x -> x

one :: Nat
one = \f x -> f x

two :: Nat
two = \f x -> f (f x)

three :: Nat
three = \f x -> f (f (f x))

-- Successor Function
succ :: Nat -> Nat
succ n = \f x -> f (n f x)

-- Addition
add :: Nat -> Nat -> Nat
add m n = \f x -> m f (n f x)

-- Multiplication
mul :: Nat -> Nat -> Nat
mul m n = \f x -> m (n f) x

-- Exponentiation
exp :: Nat -> Nat -> Nat
exp m n = \f -> n m f

-- Pair Construction (for Predecessor Function)
pair :: a -> b -> (a -> b -> c) -> c
pair x y f = f x y

fst :: ((a -> b -> a) -> a) -> a
fst p = p (\x _ -> x)

snd :: ((a -> b -> b) -> b) -> b
snd p = p (\_ y -> y)

-- Predecessor Function using Pair
pred :: Nat -> Nat
pred n = fst (n (\x -> pair (snd x) (succ (snd x))) (pair zero zero))

-- Subtraction
sub :: Nat -> Nat -> Nat
sub m n = n pred m

-- Less than or equal
leq :: Nat -> Nat -> Bool
leq m n = iszero (sub m n)

-- Test if Zero
iszero :: Nat -> Bool
iszero n = n (\_ -> False) True

-- Ackermann Function
ack :: Nat -> Nat -> Nat
ack = \m n -> m (\f -> n f (f one)) succ
