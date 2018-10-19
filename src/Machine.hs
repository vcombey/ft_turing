module Machine
  (
    execute
  ) where

import Program
import Tape

changeCell :: Direction -> Cell -> Cell
changeCell Program.Right i = i + 1
changeCell Program.Left i = i - 1

data Step = Step {
    tape :: Tape
    , cell :: Cell
    , state :: State
    , transition :: Transition
} deriving Show

data Output = 
   Blocked ([Step], Tape, Cell)
  | Halt ([Step], Tape, Cell)
 
 deriving Show


execute :: Tape -> Program.Program -> Output
execute tape program =
  aux tape program 0 (initial program) []
  where aux tape program cell state acc_step = if state `elem` (finals program) then Halt (acc_step, tape, cell)
        else (let curr_symb = readSymbol cell (blank program) tape in
            case getTransition program state curr_symb of
            Nothing -> Blocked (acc_step, tape, cell)
            Just transition -> (
                let step = Step {tape = tape, cell = cell, state = state, transition = transition} in
                let new_tape = putSymbol cell (write transition) tape in
                    let new_cell = changeCell (action transition) cell in
                        let new_state = to_state transition in
                            aux new_tape program new_cell new_state (step:acc_step)))
  
  

--PROGRAM
