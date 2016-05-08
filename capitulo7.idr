module capitulo7

-- Generic Comparisons with Eq and Ord

data Shape = Triangle Double Double
             | Rectangle Double Double
             | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius


-- 1. Implement Eq for Shape
Eq Shape where
    (==) (Triangle x z) (Triangle y w) = (x == y) && (z == w)
    (==) (Rectangle x z) (Rectangle y w) = (x == y) && (z == w)
    (==) (Circle x) (Circle y) = x == y
    (==) _ _ = False
    -- (/=) x y = not (x == y) Esta viene definida por defecto en base a la implementacion de (==)

-- 2. Implement Ord for Shape. Shapes should be ordered by area, so that shapes with a larger area are considered greater
--    than shapes with a smaller area.
Ord Shape where
    compare x y = compare (area x) (area y)
{- Estas se definen en por defecto en base a (==) y compare
    (<) x y = ?Ord_rhs_2
    (>) x y = ?Ord_rhs_3
    (<=) x y = ?Ord_rhs_4
    (>=) x y = ?Ord_rhs_5
    max x y = ?Ord_rhs_6
    min x y = ?Ord_rhs_7
-}

-- Interfaces defined in the Prelude

data Expr num = Val num
                | Add (Expr num) (Expr num)
                | Sub (Expr num) (Expr num)
                | Mul (Expr num) (Expr num)
                | Div (Expr num) (Expr num)
                | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

-- 1. Implement Show for the Expr type.
Show num => Show (Expr num) where
    show (Val x) = show x
    show (Add x y) = show x ++ " + " ++  show y
    show (Sub x y) = show x ++ " - " ++ show y
    show (Mul x y) = show x ++ " * " ++ show y
    show (Div x y) = show x ++ " div " ++ show y
    show (Abs x) = "|" ++ show x ++ "|"

-- 2. Implement Eq for the Expr type. Expressions should be considered equal if their evaluation is equal.
(Eq num, Integral num, Neg num) => Eq (Expr num) where
    (==) x y = eval x == eval y

-- 3. Implement Cast to allow conversions from Expr num to any appropiately constrained type num
(Integral num, Neg num) => Cast (Expr num) num where
    cast orig = eval orig

-- Interfaces Parameterised by Type -> Type

-- 1. Implement Functor for Expr.
Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add x y) = Add (map func x) (map func y)
    map func (Sub x y) = Sub (map func x) (map func y)
    map func (Mul x y) = Mul (map func x) (map func y)
    map func (Div x y) = Div (map func x) (map func y)
    map func (Abs x) = Abs (map func x)

-- 2. Implement Eq and Foldable for Vect.
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

Eq a => Eq (Vect n a) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
    foldr f x [] = x
    foldr f x (y :: ys) = f y (foldr f x ys)
--  foldl viene de regalo, si esta implementado foldlr
