-- Ejercicios Capitulo 2

-- Ejercicio 1 Facil

-- (String,String,String)
-- List String
-- ((Char, String), Char)

-- Ejercicio 2 Facil

palindrome : String -> Bool
palindrome str = str == (reverse str)

-- Ejercicio 3
palindrome2 : String -> Bool
palindrome2 str = toLower str == ((reverse . toLower) str)

-- Ejercicio 4 Facil

palindrome3 : String -> Bool
palindrome3 str = if length str > 10 then palindrome2 str else False

-- Ejercicio 5 Facil

palindrome4 : Nat -> String -> Bool
palindrome4 num str = if length str > num then palindrome2 str else False

-- Ejercicio 6 Facil

counts : String -> (Nat, Nat)
counts str = ((length . words) str, length str)

-- Ejercicio 7 Facil

top_ten : Ord a => List a -> List a
top_ten = (Prelude.List.take 10) . reverse . sort

-- Ejercicio 8 Facil

over_length : Nat -> List String -> Nat
over_length num list = length (filter ((>num) . length) list)

-- Ejercicio 9 Facil
-- Debe ser puesto en un fichero aparte para funcionar
showPalindrome : String -> String
showPalindrome str = "The string is palindrome: " ++ show (palindrome2 str)
palindromeREPL : IO ()
palindromeREPL = repl "Enter a String: "
                 showPalindrome

-- Debe ser puesto en un fichero aparte para funcionar
showCounts : String -> String
showCounts str = "counts(str) = " ++ show (counts str)
countsREPL : IO ()
countsREPL = repl "Enter a String: "
             showCounts
