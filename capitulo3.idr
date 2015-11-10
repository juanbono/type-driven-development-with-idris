-- Ejercicios capitulo 3
import Data.Vect

myLength : List _ -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = (myReverse xs) ++ [x]

myMap1 : (a -> b) -> List a -> List b
myMap1 f [] = []
myMap1 f (x :: xs) = f x :: (myMap1 f xs)

myMap2 : (a -> b) -> Vect n a -> Vect n b
myMap2 f [] = []
myMap2 f (x :: xs) = f x :: myMap2 f xs


-- Operaciones Matriciales.

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = sumarColumnas x y :: (addMatrix xs ys)
                                where
                                sumarColumnas : Num a => Vect n a ->
                                                         Vect n a ->
                                                         Vect n a
                                sumarColumnas x y = zipWith (+) x y
