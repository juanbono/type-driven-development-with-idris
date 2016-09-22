-- Capitulo 10: Views

-- Ejercicios 10.1.6
-- 1. The TakeN view allows traversal of a list several elements at a time:
data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs

-- The Fewer constructor covers the case where there are fewer than n
-- n elements. Define the covering function takeN

-- To check that your definition works, you should be able to run the following
-- function, which groups lists into sublists with n elements each
groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-- 2. Use TakeN to define a function which splits a list into two halves
--    by calculating its length:
halves : List a -> (List a, List a)

-- (Hint: Use div for dividing a Nat)

-- Ejercicios 10.2.5
-- 1. Implement a function equalSuffix, using the SnocList view defined
--    Data.List.Views. It should have the following type:
equalSuffix : Eq a => List a -> List a -> List a

-- 2. Implement mergeSort for vectors, using the SplitRec view defined
--    in Data.Vect.Views

-- 3. Write a function toBinary which converts a Nat to String containing
--    a binary representation of the Nat. You should use the HalfRec view
--    defined in Data.Nat.Views.
-- (Hint: It's okay to return an empty string if the input is Z.)

-- 4. Write a function palindrome which returns whether a list is the same
--    when traversed forwards or backwards, using the VList view defined in
--    Data.List.Views
-- (Hint: The VList view allows traversing a list, in linear time, processing
--        the first and last elements simultaneosly and recursing on the middle
--        of the list.)
-- For each of these exercises, make sure Idris considers your solution to be
-- total.


-- Ejercicios 10.3.4
-- 1. Write a function getValues which returns a list of all of the values in
--    a DataStore. It should have the following type:
getValues : DataStore (SSTring .+. val_schema) -> List (SchemaType val_schema)

-- 2. Define a view which allows other modules to inspect the abstract Shape
--    data type. You Should be able to use it to complete the following def:

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = ?area_rhs_1
  area (rectangle width height) | SRectable = ?area_rhs_2
  area (circle radius) | SCircle = ?area_rhs_3
