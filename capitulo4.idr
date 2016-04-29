module capitulo4

data Shape = Triangle Double Double
             | Rectangle Double Double
             | Circle Double

data Picture = Primitive Shape
               | Combine Picture Picture
               | Rotate Double Picture
               | Translate Double Double Picture
               
data Tree elem = Empty
                 | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x t@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => t
                                      GT => Node left val (insert x right)

-- Ejercicios Capitulo 4
-- 1 listToTree inserts every element of a list into a bst
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = ?holas

-- 2 treeToList flattens a tree into a list using inorder traversal.
treeToList : Tree a -> List a

-- 3 Define a recursive data type Expr for arithmetic expressions.
-- TODO

-- 4 Write a function evaluate which evaluates an Expr
evaluate : Expr -> Int

-- 5 Write a function maxMaybe which returns the larger of two inputs or Nothing if both inputs are Nothing
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a

-- 6 Write a function biggestTriangle which returns the area of the biggest triangle in a picture or Nothing if there are no triangles.
biggestTriangle : Picture -> Maybe Double
