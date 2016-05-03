module capitulo6

import Data.Vect

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType => (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

data Format = Number Format | Str Format | Lit String Format | Double' Format | Char' Format | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Double' fmt) = (d : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Char' fmt) = (char : Char) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Double' fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Char' fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Char' (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Double' (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

-- Defining functions with variable numbers of arguments

-- 1. An n x m matrix can be represented by nested vectors of Double.
--    Define a type synonym Matrix : Nat -> Nat -> Type
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

-- 2. Extend printf to support formatting directives for Char and Double.
-- terminado :)

-- 3. We could implement a vector as nested pairs, with the nesting calculated
--    from the length. Define a type level function TupleVect.
TupleVect : (length : Nat) -> (t : Type) -> Type
TupleVect Z t = ()
TupleVect (S k) t = (t, TupleVect k t)

test : TupleVect 4 Nat
test = (1,2,3,4,())

-- Enhancing the Interactive DataStore with Schemas

data DataStore : Type -> Type where
  MkData : (size : Nat) -> (items : Vect size schema) -> DataStore schema

-- 1. Update the data store program to support Chars in the schema.
-- TODO

-- 2. Modify the get command so that, if given no arguments, it prints the entire contents of the data store.
-- TODO

-- 3. Update the data store program so that it uses do notation rather than nested case blocks where appropiate.
-- TODO



