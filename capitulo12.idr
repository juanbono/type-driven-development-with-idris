module capitulo12
import Control.Monad.State

-- 12.1.5 Exercises
-- 1. Write a function which updates a state by applying a function to the current state
update : (stateType -> stateType) -> State stateType ()
update f = do x <- get
              put (f x)
-- you should be able to use update to reimplement increase:
increase : Nat -> State Nat ()
increase x = update (+x)

-- 2. Write a function which uses State to count the number of occurrences of Empty in a tree.
--    It should have the following type:
data Tree a = Empty
            | Node (Tree a) a (Tree a)

count : Tree a -> State Nat ()
count Empty = increase 1
count (Node left val right) = ST ?srasa1

-- 3. Write a function which counts the number of occurrences of both Empty and Node in a tree,
--    using State to store the count of each in a pair. It should have the following type:
countEmptyNode : Tree a -> State (Nat, Nat) ()

-- 12.3.7 Exercises
-- 1. Write a function updateGameState with the following type:
-- updateGameState : (GameState -> GameState) -> Command ()

-- 2. Implement the Functor, Applicative and Monad interfaces for Command

-- 3. We could define records for representing an article on a social news web site as follows,
--    along with the number of times it has been upvoted or downvoted:
record Votes where
       constructor MkVotes
       upvotes : Integer
       downvotes : Integer

record Article where
       constructor MkArticle
       title : String
       url : String
       score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

-- Write a function to calcule the overall score of a given article, where the score is calculated
-- form the number of downvotes subtracted from the number of upvotes. It should have the following type:
getScore : Article -> Integer

-- test it with:
badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

-- 4. Write a function addUpvote and addDownvote which modify an article's score up or down.
--    They should have the following types:
addUpvote : Article -> Article

addDownvote : Article -> Article


