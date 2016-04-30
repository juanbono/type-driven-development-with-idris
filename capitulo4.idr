module capitulo4
import Data.Vect

-- Defining Data Types

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

-- Defining Dependent Data Types
-- 1 Extend the Vehicle data type so that it supports unicyles and motorcycles
-- 2 Extend the PowerSource and Vehicle data types to support electric vehicles
data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : (bat : Nat) -> Vehicle Electricity
  Tram : (bat : Nat) -> Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (ElectricCar bat) = 4
wheels (Tram bat) = 0

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

-- 3 what is an appropiate type for the Vect.take function
myTake : (n : Nat) -> Vect (n + m) a -> Vect n a

-- 4 Implement the take function on Vect
myTake Z xs = []
myTake (S k) (x :: xs) = x :: myTake k xs

-- 5 Write a function sumEntries with the following type:
-- It should return the sum of the entries at position pos in each of the
-- inputs if pos is within bounds, or Nothing otherwise.

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos x y = case integerToFin pos n of
                              Nothing => Nothing
                              Just i => Just (index i x + index i y)


-- Type-driven Implementation of an Interactive Data Store

-- 1 Add a size command which displays the number of entries in the store
-- 2 Add a search command which displays all of the entries in the store containing a given substring
-- 3 Extend search to print the index of each result, as well as the string.
-- TODO

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String | Get Integer | Quit | Size | Search String

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just id => Just (index id (items store) ++ "\n", store)

getMatchedEntries : (substring : String) -> (store : DataStore) -> Maybe (String, DataStore)
getMatchedEntries substring store = let entries = (filter (isInfixOf substring) (items store)) in
                                        case fst entries of
                                             Z => Just ("No matched entries\n", store)
                                             (S k) => Just (formatEntries (snd entries), store)
                                        where
                                          formatEntries : Vect n String -> String
                                          formatEntries {n = (S k)} v = "Entries: \n" ++ foldl1 (\s1, s2 => s1 ++ "\n" ++ s2) v ++ "\n"

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (Get pos) => getEntry pos store
                                Just Quit => Nothing
                                Just Size => Just ("The store has " ++ show (size store) ++ " entries\n", store)
                                Just (Search str) => getMatchedEntries str store
main : IO ()
main = replWith (MkData _ []) "Command: " processInput
