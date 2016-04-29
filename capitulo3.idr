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
Matrix : Nat -> Nat -> Type -> Type
Matrix n m t = Vect n (Vect m t) -- Matrices de nxm con elementos de tipo t

-- create_empties usando n como argumento implicito.
create_empties : Matrix n 0 a
create_empties {n = Z} = []
create_empties {n = (S k)} = [] :: create_empties

transpose_mat : Matrix m n a -> Matrix n m a
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          zipWith (::) x xs_trans

addMatrix : Num a => Matrix n m a-> Matrix n m a -> Matrix n m a
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = sumarColumnas x y :: (addMatrix xs ys)
                                where
                                sumarColumnas : Num a => Vect n a -> Vect n a -> Vect n a
                                sumarColumnas x y = zipWith (+) x y

productoPunto : Num a => (x : Vect m a) -> (y : Vect m a) -> a
productoPunto [] y = 0
productoPunto (x :: xs) (y :: ys) = x * y + productoPunto xs ys

vxm : Num a => (x : Vect m a) -> (rightM : Matrix p m a) -> Vect p a
vxm x [] = []
vxm x (y :: ys) = productoPunto x y :: vxm y ys

multMatrix : Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
multMatrix [] y = []
multMatrix (x :: xs) y = let rightM = transpose_mat y in
                                      (vxm x rightM) :: multMatrix xs y
