module capitulo5

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
-- guess : (target : Nat) -> IO () and it should behave as follows:
-- a. Repeatedly ask the user to guess a number, and display whether the guess too high, too low, or correct.
-- b. When the guess is correct, exit.
-- TODO

-- 2. Implement a main function which chooses a random number between 1 and 100 then calls guess.
-- TODO

-- 3. Extend guess so that it counts the number of guesses the user has taken, and displays it before the input is read.
-- TODO

-- 4. Implement your own versions of repl and replWith.
-- TODO

-- Reading and Validating Dependent Types

-- 1. Write a function readToBlank : IO (List String) which reads input from the console until the user enters a blank line.
-- TODO

-- 2. Write a function readAndSave : IO () which reads input from the console until the user enters a blank line, then reads a file name from
--    the console, and writes the input to that file.
-- TODO

-- 3. Write a function readVectFile : (filename : String) -> IO (n ** Vect n String) which reads the contents of a file into a dependent pair
--    containing a length and a Vect of that length. If there are any errors, it should return an empty vector.
