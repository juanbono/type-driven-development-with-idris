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

infixr 5 .+.

data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => case parseSchema xs of
                                              Nothing => Nothing
                                              Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                 [] => Just SInt
                                 _ => case parseSchema xs of
                                           Nothing => Nothing
                                           Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') => case parsePrefix schemar input' of
                                                                            Nothing => Nothing
                                                                            Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restOk => Just (Add restOk)

parseCommand schema "get" val = case all isDigit (unpack val) of
                                     False => Nothing
                                     True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do schemaOk <- parseSchema (words rest)
                                       Just (SetSchema schemaOk)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)


addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newItem = MkData schema _ (addToData store)
  where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newItem]
    addToData (item :: items) = item :: addToData items

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                     case integerToFin pos (size store) of
                          Nothing => Just ("Out of range\n", store)
                          Just id => Just (display (index id (items store)) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Just (SetSchema schema') => case setSchema store schema' of
                                                                 Nothing => Just ("Can't update schema\n", store)
                                                                 Just store' => Just ("Ok\n", store')
                                Just (Get pos) => getEntry pos store
                                Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput


-- 1. Update the data store program to support Chars in the schema.
-- TODO

-- 2. Modify the get command so that, if given no arguments, it prints the entire contents of the data store.
-- TODO

-- 3. Update the data store program so that it uses do notation rather than nested case blocks where appropiate.
-- TODO
