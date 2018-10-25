module Machine
  (
    execute
  ) where

import qualified Data.List as List
import Program
import Tape

changeCell :: Direction -> Cell -> Cell
changeCell Program.Right i = i + 1
changeCell Program.Left i = i - 1
changeCell Program.None i = i

data Step = Step {
    tape :: Tape
    , cell :: Cell
    , state :: State
    , transition :: Transition
} deriving Show

prettyStep :: Step -> Program -> String
prettyStep s p = prettyTape (tape s) (blank p) (cell s) ++ " "
  ++ prettyTransition (state s) (transition s) ++ "\n"

execute :: Tape -> Program.Program -> IO ()
execute tape program =
  do
    aux tape program 0 (initial program) []
  where
    aux tape program cell state acc_step =
     if state `elem` (finals program)
     then do
       putStr $ prettyTape tape (blank program) cell
       putStrLn ("\nThe Turing Machine Succeed in " ++ (show $ length acc_step) ++ " step")
     else (let curr_symb = readSymbol cell (blank program) tape in
         case getTransition program state curr_symb of
         Nothing -> do putStr $ prettyTape tape (blank program) cell
                       putStrLn "\nThe Turing Machine had Segmentation Fault"
         Just transition ->
             let step = Step {tape = tape, cell = cell, state = state, transition = transition} in
             let new_tape = putSymbol cell (write transition) tape in
             let new_cell = changeCell (action transition) cell in
             let new_state = to_state transition in
               do
                putStr $ prettyStep step program
                aux new_tape program new_cell new_state (step:acc_step))
