-- 11.1.7 exercises
import Data.Primitives.Views

-- 1. Write a function every_other which produces a new Stream from every second element of an input Stream
every_other : Stream a -> Stream a
every_other (x1::x2::xs) = x2 :: every_other xs

-- 2. Write an implementation of Functor for InfList.

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

Functor InfList where
  map f (x::xs) = f x :: map f xs

-- 3. Define a data type Face which represents the faces of a coin,
--    heads  or tails. Then define:
data Face = Head | Tail

total
getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) = case rem of
                                                 0 => Head
                                                 _ => Tail
coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = ?coinFlips_rhs_1
coinFlips (S k) xs = ?coinFlips_rhs_2

-- (Hint: It will help to define a function getFace : Int -> Face)

-- 4. We can define a function to calculate the square root of a Double as follows:

--  1. Generate a sequence of closer approximations to the square root
--  2. Take the first approximation which, when squared, is within a desired bound from the original number.

-- Write a function which generates a sequence of approximations
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double

-- Starting from an approximation approx you can generate the next approximation using the formula
-- next = (approx + (number / approx)) / 2

-- 5. Write a function which finds the first approximation of a square root which is within a desired bound,
--    or within a maximum number of iterations:


square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double

-- This should return the first element of approxs if max is zero, of if, when that element is squared, the difference between it and number is within bound.

-- If you have implemented these correctly, you should be able to define square_root as follows and it should be total:
--square_root : (number : Double) -> Double
--square_root number = square_root_bound 100 number 0.0000000000.1 (square_root_approx number number)

-- 11.2.7 Exercise
-- The repl function defined in the Prelude is not total because it is an IO action which loops indefinitely. Implement a new version of repl using InfIO:
--totalREPL : (prompt : String) -> (action : String -> String) -> InfIO


-- 11.3.4 Exercises
-- 1. Update quiz so that it keeps track of the total number of questions, and returns the number of correct answers and the number of questions attempted.

-- 2. Extend the Command type so that it also supports reading and writing files.
-- (Hint: Look at the types of readFile and writeFile in the Prelude to decide what types your data constructors should have)

-- 3. Use your extended Command type to implement an interactive shell which supports the following commands:
--    1. cat [filename] which reads a file and displays it's contents
--    2. copy [source] [destination] which reads a source file, and writes it's contents to a destination file.
--    3. exit which exits the shell.
