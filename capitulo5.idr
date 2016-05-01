module capitulo5

import System
import Data.Vect

-- Interactive Programming with IO

-- 1. Using do notation, write a program which reads two strings then displays the length of the longer string.
displayLongerStringDoVersion : IO ()
displayLongerStringDoVersion = do putStr "Write 2 words: "
                                  str1 <- getLine
                                  str2 <- getLine
                                  let longer = if length str1 > length str2 then str1 else str2
                                  putStrLn (show (length longer))

-- 2. Write the same program usin >>= instead of do notation.
displayLongerStringBindVersion : IO ()
displayLongerStringBindVersion = putStr "Write 2 words: " >>= \_ =>
                                 getLine >>= \str1 =>
                                 getLine >>= \str2 =>
                                 let longer = if length str1 > length str2 then str1 else str2 in
                                 putStrLn (show (length longer))

-- Interactive Programs and Control Flow

-- 1. Write a function which implements a simple "guess the number" game. It should have the following type:
--    guess : (target : Nat) -> IO () and it should behave as follows:
--    a. Repeatedly ask the user to guess a number, and display whether the guess too high, too low, or correct.
--    b. When the guess is correct, exit.

readNumber : String -> Maybe Nat
readNumber str = if all isDigit (unpack str) then (Just (cast str)) else Nothing

showAndDo : String -> IO () -> IO ()
showAndDo str action = do putStr str
                          action

guess : (target : Nat) -> IO ()
guess target = do putStr "Guess a number: "
                  input <- getLine
                  case readNumber input of
                       Just number => case compare target number of
                                           EQ => pure ()
                                           LT => showAndDo "Too high, try again\n"  (guess target)
                                           GT => showAndDo "Too low, try again\n" (guess target)
                       Nothing => showAndDo "Wrong input, try again\n" (guess target)


-- 2. Implement a main function which chooses a random number between 1 and 100 then calls guess.
main : IO ()
main = do number <- time
          let target = (mod (toNat number) 100) + 1
          guess target

-- 3. Extend guess so that it counts the number of guesses the user has taken, and displays it before the input is read.
extendedGuess : (target : Nat) -> (guesses : Nat) -> IO ()
extendedGuess target guesses = do putStr "Number of guesses: "
                                  printLn guesses
                                  putStr "Guess a number: "
                                  input <- getLine
                                  case readNumber input of
                                       Just number => case compare target number of
                                            EQ => pure ()
                                            LT => showAndDo "Too high, try again\n" (extendedGuess target (S guesses))
                                            GT => showAndDo "Too low, try again\n" (extendedGuess target (S guesses))
                                       Nothing => showAndDo "Wrong input, try again\n" (extendedGuess target (S guesses))

extendedMain : IO ()
extendedMain = do number <- time
                  let target = (mod (toNat number) 100) + 1
                  extendedGuess target 0

-- 4. Implement your own versions of repl and replWith.
myRepl : String -> (String -> String) -> IO ()
myRepl str f = do putStr str
                  arg <- getLine
                  putStrLn (f arg)
                  myRepl str f

myReplWith : (state : a) ->  (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     case onInput state prompt of
                                          Nothing => pure ()
                                          Just (msg, newState) => myReplWith newState msg onInput
-- ** Nota: Esta funcion pasa el type checker pero no la probe para ver si es correcta. Probar **

-- Reading and Validating Dependent Types

-- 1. Write a function readToBlank : IO (List String) which reads input from the console until the user enters a blank line.
isBlank : String -> Bool
isBlank str = all isSpace (unpack str)

readToBlank : IO (List String)
readToBlank = do input <- getLine
                 case isBlank input of
                      False => (input ::) <$> readToBlank
                      True => pure []

-- 2. Write a function readAndSave : IO () which reads input from the console until the user enters a blank line, then reads a file name from
--    the console, and writes the input to that file.
readAndSave : IO ()
readAndSave = do list <- readToBlank
                 putStr "Save it to: "
                 filePath <- getLine
                 let contents = show list
                 res <- writeFile filePath contents
                 case res of
                      Left FileError => putStrLn "Error"
                      Right () => pure ()

-- 3. Write a function readVectFile : (filename : String) -> IO (n ** Vect n String) which reads the contents of a file into a dependent pair
--    containing a length and a Vect of that length. If there are any errors, it should return an empty vector.
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = ?readVectFile_rhs
-- Esta es hardcore
