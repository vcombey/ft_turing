module Machine
  (
    TuringMachine
    , check
    , new
--    , executeMachine
  ) where

import Data.IntMap.Strict as IntMap


-- RUBAN
-- EXECUTE
changeState :: Direction -> Case -> Case
changeState Right i = i + 1
changeState Left i = i - 1

data MachineOutput = MachineOutput
  | Res Tape
  | Halt

execute :: Tape -> Program -> Tape 

--PROGRAM
