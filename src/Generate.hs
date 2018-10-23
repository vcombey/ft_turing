{-# LANGUAGE DeriveGeneric #-}
module Generate
  (
  universal
  , name
  ) where

import Data.HashMap.Strict as HashMap
import GHC.Generics
import Data.Aeson
import Data.Function
import qualified Data.List as List
import Program

type Machine = HashMap State [Transition]
type StateInt = Int

failureState = -1

globalAlphabet = ["0" , "1" , "X", "Y", "Z", "B"]
globalBlank = "0"

for_letter ::  [Symbol] -> Transition -> [Transition]
for_letter list_symb trans =
  List.map (\symb -> if Program.write trans /= "" then trans { Program.read = symb } else trans { Program.read = symb, Program.write = symb }) list_symb


not_letter :: [Symbol] -> Transition -> [Transition]
not_letter list_not_symb =
  for_letter (globalAlphabet List.\\ list_not_symb)

any_letter = not_letter []

newTransition :: Symbol -> Symbol -> Direction -> StateInt -> Transition
newTransition read write action state =
  Transition {
    Program.read = read
  , Program.write = write
  , to_state = show state
  , action = action
  }

transition_state first_state final_state =
    HashMap.empty &
    HashMap.insert (show (first_state))
            (for_letter globalAlphabet (newTransition "" "" Program.None final_state))

maxState :: Machine -> StateInt
maxState m = maximum (List.map Prelude.read (keys m))

(==>) :: (StateInt, StateInt) -> (StateInt -> Machine) -> Machine
(==>) (first_state, nb_state) f =
    transition_state first_state (first_state + nb_state) &
    union (f (first_state + nb_state))

(===>) :: (Machine, StateInt) -> (StateInt -> Machine) -> Machine
(===>) (machine, first_state) f =
    transition_state first_state (maxState machine + 1) &
    union (f (maxState machine + 1)) & union machine


find_first_until :: Direction -> Symbol -> [Symbol] -> StateInt -> StateInt -> StateInt -> Machine
find_first_until dir alpha until success_state failure_state first_state =
    HashMap.empty &
    HashMap.insert (show (first_state))
            ([(newTransition alpha alpha None success_state)]
            ++ (for_letter until (newTransition "" "" Program.None failure_state))
            ++ not_letter (until ++ [alpha]) (newTransition "" "" dir (first_state)))

find_first :: Direction -> Symbol -> StateInt -> StateInt -> Machine
find_first dir alpha success_state first_state =
  find_first_until dir alpha [] success_state failureState first_state

replace_first_until :: Direction -> Symbol -> Symbol -> [Symbol] -> StateInt -> StateInt -> StateInt -> Machine
replace_first_until dir alpha beta until success_state failure_state first_state =
    (first_state, 2) ==> find_first_until dir alpha until (first_state + 1) failure_state &
    HashMap.insert (show (first_state + 1))
            ([(newTransition alpha beta None success_state)]
            ++ not_letter ([alpha]) (newTransition "" "" dir (failure_state)))

replace_first :: Direction -> Symbol -> Symbol -> StateInt -> StateInt -> Machine
replace_first dir alpha beta success_state first_state =
    replace_first_until dir alpha beta [] success_state failureState first_state

replace_all :: Direction -> Symbol -> Symbol -> [Symbol] -> StateInt -> StateInt -> Machine
replace_all dir alpha beta until success_state first_state =
  replace_first_until dir alpha beta until first_state success_state (first_state)

type FunctionMachine = (StateInt -> StateInt -> StateInt -> Machine)

compose_function :: FunctionMachine -> FunctionMachine -> StateInt -> StateInt -> StateInt -> Machine
compose_function f1 f2 global_success global_failure first_state = 
     (first_state, 2) ==> f1 (first_state + 1) global_failure & \m ->
     (m, first_state + 1) ===> f2 (global_success) (global_failure)
    
copy_machine :: StateInt -> StateInt -> Machine
copy_machine success_state first_state =
  ((first_state, 4) ==> replace_first_until Program.Right "1" "B" ["Y", globalBlank] (first_state + 1) (first_state + 3)) & \m ->
  (m, first_state + 1) ===> compose_function (find_first_until Program.Right "Y" []) (replace_first_until Program.Right "0" "1" []) (first_state + 2) failureState & \m ->
  (m, first_state + 2) ===> find_first Program.Left "B" (first_state) & \m ->
  (m, first_state + 3) ===> replace_all Program.Left "B" "1" ["X"] success_state

copy_machine_rev :: StateInt -> StateInt -> Machine
copy_machine_rev success_state first_state =
  ((first_state, 4) ==> find_first Program.Right "Y" (first_state + 1) & \m ->
  (m, first_state + 1) ===> replace_first_until Program.Right "1" "B" [globalBlank] (first_state + 2) (first_state + 3)) & \m ->
  (m, first_state + 2) ===> compose_function (find_first_until Program.Left "X" []) (replace_first_until Program.Right "0" "1" []) (first_state) failureState & \m ->
  (m, first_state + 3) ===> replace_all (Program.Left) "B" "1" ["Y"] success_state

matching_machine :: StateInt -> StateInt -> StateInt -> Machine
matching_machine success_state failure_state first_state =
  ((first_state, 7) ==> replace_first_until Program.Right "1" "B" ["Y", globalBlank] (first_state + 1) (first_state + 4)) & \m ->
  (m, first_state + 1) ===> find_first Program.Right "Y" (first_state + 2) & \m ->
  (m, first_state + 2) ===> replace_first_until Program.Right "1" "B" [globalBlank] (first_state + 3) (first_state + 5) & \m ->
  (m, first_state + 3) ===> find_first Program.Left "X" (first_state)  & \m ->
  (m, first_state + 4) ===> compose_function (find_first_until Program.Right "Y" []) (find_first_until Program.Right "1" [globalBlank]) (first_state + 5) (first_state + 6) & \m ->
  (m, first_state + 5) ===> replace_all Program.Left "B" "1" ["X"] failure_state & \m ->
  (m, first_state + 6) ===> replace_all Program.Left "B" "1" ["X"] success_state


universal = 
--  let trans = find_first_until Program.Right "X" [] 0 1 2 in
--    let trans = replace_first_until Program.Right "X" "B" [] 0 1 2 in
--    let trans = replace_all Program.Right "X" "B" ["Z"] 0 1 in
--    let trans = copy_machine 0 1 in
    let trans = matching_machine 0 1 2 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet ++ ["."]
        , blank = "."
        , states = List.map (\x -> show x) [-1..(maxState trans + 1)]
        , initial = "2"
        , finals = []
        , transitions = trans
    }
