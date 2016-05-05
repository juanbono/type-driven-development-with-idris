module capitulo9
import Data.Vect

-- Membership Tests: The Elem Predicate

-- 1. Data.List includes a version of Elem for List which works similarly to Elem for Vect. How would you define it?
-- TODO

-- 2. The following predicate states that a specific value is the last value in a List:

data Last : List a -> a -> Type where
  LastOne : Last [item] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value


-- Write a function isLast which decides whether a value is the last element in a List. It should have the following type:
isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
