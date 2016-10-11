module capitulo13

-- 1. Change the RingBell operation so that it works in any state,
--    rathen than only when the door is closed.
-- Check if this function type checks:
doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do RingBell
              Open
              RingBell
              Close

-- 2. The following (incomplete) type defines a command for guessing
--    game, where the input and output states are the number of
--    remaining guesses allowed:
data GuessCmd : Type -> Nat -> Nat -> Type where
  Try : Integer -> GuessCmd Ordering ?in_state ?out_state

  Pure : ty -> GuessCmd ty state state
  (>>=) : GuessCmd a state1 state2 ->
          (a -> GuessCmd b state2 state3) ->
          GuessCmd b state1 state3

-- Complete the type of Try so that you can only make a guess when
-- there is at least one guess allowed, and guessing reduces the
-- number of guesses available.
-- If you have a correct answer, the following definition should type
-- check:
three_guesses : GuessCmd () 3 0
three_guesses = do Try 10
                   Try 20
                   Try 15
                   Pure ()
-- also, the following def should not type check:
no_guesses : GuessCmd () 0 0
no_guesses = do Try 10
                Pure ()

-- 3. The following type defines the possible states of matter
data Matter = Solid | Liquid | Gas

-- Define a type MatterCmd in such way that the following definitions
-- type check:
ice_steam : MatterCmd () Solid Gas
ice_steam = do Melt
               Boil

steam_ice : MatterCmd () Gas Solid
steam_ice = do Condense
               Freeze

-- Additionally, the following definition should not type check:
over_melt : MatterCmd () Solid Gas
over_melt = do Melt
               Melt

-- 12.2.4 Exercises
-- 1. Add user commands to the stack-based calculator for subtract
--    and multiply.

-- 2. Add a user command negate to the stack based calculator for negating
--    the top item on the stack.

-- 3. Add a user command discard, which removes the top item from the stack

-- 4. Add a user command duplicate, which duplicates the top item on the stack

