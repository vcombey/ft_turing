module Machine
  (
    TuringMachine
    , check
    , new
--    , executeMachine
  ) where

import Data.IntMap.Strict as IntMap


-- RUBAN
data Ruban = IntMap Char

readSymbol :: Key -> Ruban -> Ruban
readSymbol num ruban = IntMap.lookup num ruban

putSymbol :: Key -> a -> Ruban -> Ruban
putSymbol num symbol ruban = IntMap.insert num symbol ruban

fromInitialState :: String -> Ruban
fromInitialState = fold (\acc \symb \i -> putSymbol i symb acc) IntMap.empty


-- EXECUTE
changeState :: Direction -> Case -> Case
changeState Right i = i + 1
changeState Left i = i - 1

data MachineOutput = MachineOutput
  | Res Ruban
  | Halt

execute :: Ruban -> Program -> Ruban 

--PROGRAM
