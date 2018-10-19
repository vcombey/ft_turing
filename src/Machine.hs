module Machine
  (
  ) where

import Program
import Tape

changeState :: Direction -> Cell -> Cell
changeState Program.Right i = i + 1
changeState Program.Left i = i - 1

data MachineOutput = MachineOutput
  | Res Tape
  | Halt


data Step = Step {
    tape :: Tape
    , cell :: Cell
    , state :: State
    , transition :: Transition
}

--execute :: Tape -> Program -> [Step] 

--PROGRAM
