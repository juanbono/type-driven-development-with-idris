module capitulo4

data Shape = Triangle Double Double
             | Rectangle Double Double
             | Circle Double

%name Shape shape, shape1, shape2, shape3

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
               | Combine Picture Picture
               | Rotate Double Picture
               | Translate Double Double Picture

%name Picture picture, picture1, picture2, picture3

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1, tree2, tree3

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => t
                                      GT => Node left val (insert x right)

-- Ejercicios Capitulo 4
-- 1 listToTree inserts every element of a list into a bst
listToTree : Ord a => List a -> Tree a
listToTree lst = foldl (flip insert) Empty lst

-- 2 treeToList flattens a tree into a list using inorder traversal.
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = val :: (treeToList left) ++ (treeToList right)

-- 3 Define a recursive data type Expr for arithmetic expressions.
data Expr = Number Int | Plus Expr Expr | Minus Expr Expr | Times Expr Expr

-- 4 Write a function evaluate which evaluates an Expr
evaluate : Expr -> Int
evaluate (Number x)  = x
evaluate (Plus x y)  = (+) (evaluate x) (evaluate y)
evaluate (Minus x y) = (-) (evaluate x) (evaluate y)
evaluate (Times x y) = (*) (evaluate x) (evaluate y)

-- 5 Write a function maxMaybe which returns the larger of two inputs or Nothing if both inputs are Nothing
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x y = liftA2 max x y

-- 6 Write a function biggestTriangle which returns the area of the biggest triangle in a picture or Nothing if there are no triangles.
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle a b)) = Just (area t)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = liftA2 max (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x picture) = biggestTriangle picture
biggestTriangle (Translate x y picture) = biggestTriangle picture
