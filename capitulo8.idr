module capitulo8

import Data.Vect

-- Guaranteeing Equivalence of Data with Equality Types

-- 1. Implement the following function, which states that if you add the same value onto the front of equal lists,
--    the resulting lists are also equal.
--    Since this function represents an equality proof, it is sufficient to know that your definition type checks
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys

-- 2. Implement the following function, which states that if two values x and y are equal, and two lists xs and ys
--    are equal, then the two lists x :: xs and y :: ys must also be equal.
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys

-- 3. Define a type ThreeEq which expresses that three values must be equal.
ThreeEq : a -> b -> c -> Type

-- 4. Implement the following function which uses ThreeEq:
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)

-- Equality in Practice: Types and Reasoning

-- 1. Using plusZeroRightNeutral and plusSuccRightSucc, write your own version of plusCommutes:
my_plusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n

-- 2. The implementation of my_reverse we wrote earlier is inneficient, because it needs to traverse
--    the entire vector to append a single element on every iteration. We can write a better definition
--    as follows, using a helper function reverse' which takes an accumulating argument to build the
--    reversed list.
--    Complete this definition by implementing the holes.
my_reverse : Vect n a -> Vect n a
my_reverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = ?reverseProof_nil acc
        reverse' acc (x :: xs) = ?reverseProof_xs (reverse' (x :: acc) xs)

-- The Empty Type and Decidability

-- 1. Implement the following functions:
head_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void

tail_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void

-- 2. Implement DecEq for Vect. Begin with the following implementation header:
-- DecEq a => DecEq (Vect n a) where Usar definicion propia de Vect
