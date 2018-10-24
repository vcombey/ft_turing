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

globalAlphabet = ["0" , "1" , "X", "Y", "Z", "B", "."]
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
    
copy_machine :: Symbol -> Symbol -> StateInt -> StateInt -> Machine
copy_machine fromSymb toSymb success_state first_state =
  ((first_state, 4) ==> replace_first_until Program.Right "1" "B" [toSymb, globalBlank] (first_state + 1) (first_state + 3)) & \m ->
  (m, first_state + 1) ===> compose_function (find_first_until Program.Right toSymb []) (replace_first_until Program.Right "0" "1" []) (first_state + 2) failureState & \m ->
  (m, first_state + 2) ===> find_first Program.Left "B" (first_state) & \m ->
  (m, first_state + 3) ===> replace_all Program.Left "B" "1" [fromSymb] success_state

copy_machine_rev :: Symbol -> Symbol -> StateInt -> StateInt -> Machine
copy_machine_rev fromSymb toSymb success_state first_state =
  ((first_state, 4) ==> find_first Program.Right fromSymb (first_state + 1) & \m ->
  (m, first_state + 1) ===> replace_first_until Program.Right "1" "B" [globalBlank] (first_state + 2) (first_state + 3)) & \m ->
  (m, first_state + 2) ===> compose_function (find_first_until Program.Left toSymb []) (replace_first_until Program.Right "0" "1" []) (first_state) failureState & \m ->
  (m, first_state + 3) ===> replace_all (Program.Left) "B" "1" [fromSymb] success_state

matching_machine :: StateInt -> StateInt -> StateInt -> Machine
matching_machine success_state failure_state first_state =
  ((first_state, 7) ==> replace_first_until Program.Right "1" "B" ["Y", globalBlank] (first_state + 1) (first_state + 4)) & \m ->
  (m, first_state + 1) ===> find_first Program.Right "Y" (first_state + 2) & \m ->
  (m, first_state + 2) ===> replace_first_until Program.Right "1" "B" [globalBlank] (first_state + 3) (first_state + 5) & \m ->
  (m, first_state + 3) ===> find_first Program.Left "X" (first_state)  & \m ->
  (m, first_state + 4) ===> compose_function (find_first_until Program.Right "Y" []) (find_first_until Program.Right "1" [globalBlank]) (first_state + 5) (first_state + 6) & \m ->
  (m, first_state + 5) ===> replace_all Program.Left "B" "1" ["X"] failure_state & \m ->
  (m, first_state + 6) ===> replace_all Program.Left "B" "1" ["X"] success_state

shiftl_machine :: Symbol -> StateInt -> StateInt -> Machine
shiftl_machine untilSymbol success_state first_state =
  HashMap.empty &
  HashMap.insert (show first_state)
                (
                [newTransition "1" "." Program.Left (first_state + 1)
                , newTransition "0" "." Program.Left (first_state + 2)
                , newTransition "." "." Program.Left (first_state)]
                )
                &
  HashMap.insert (show (first_state + 1))
                (
                [newTransition "1" "1" Program.Left (first_state + 1)
                , newTransition "0" "1" Program.Left (first_state + 2)
                , newTransition untilSymbol untilSymbol Program.None (success_state)]
                )
                &
  HashMap.insert (show (first_state + 2))
                (
                [newTransition "1" "0" Program.Left (first_state + 1)
                , newTransition "0" "0" Program.Left (first_state + 2)
                , newTransition untilSymbol untilSymbol Program.None (success_state)]
                )
               
collapse_machine :: Symbol -> StateInt -> StateInt -> Machine
collapse_machine untilSymbol success_state first_state =
  (first_state, 3) ==> find_first_until Program.Right "1" ["0", "."] (first_state + 1) (success_state) & \m ->
  (m, first_state + 1) ===> find_first Program.Right "." (first_state + 2) & \m ->
  (m, first_state + 2) ===> shiftl_machine untilSymbol first_state

left_machine success_state first_state =
  HashMap.empty & HashMap.insert (show first_state) (any_letter (newTransition "" "" Program.Left success_state))

right_machine success_state first_state =
  HashMap.empty & HashMap.insert (show first_state) (any_letter (newTransition "" "" Program.Right success_state))

machine_with_transition :: [Transition] -> StateInt -> Machine
machine_with_transition trans first_state =
  HashMap.empty & HashMap.insert (show first_state) (trans)

shiftr_machine :: Symbol -> StateInt -> StateInt -> Machine
shiftr_machine fromSymbol success_state first_state =
  HashMap.empty &
  HashMap.insert (show first_state)
                (
                [newTransition fromSymbol fromSymbol Program.Right first_state
                , newTransition "1" "1" Program.Right (first_state + 1)
                , newTransition "0" "1" Program.Right (first_state + 2)]
                )
                &
  HashMap.insert (show (first_state + 1))
                (
                [newTransition "1" "1" Program.Right (first_state + 1)
                , newTransition "0" "1" Program.Right (first_state + 2)
                , newTransition "." "1" Program.None (success_state)]
                )
                &
  HashMap.insert (show (first_state + 2))
                (
                [newTransition "1" "0" Program.Right (first_state + 1)
                , newTransition "0" "0" Program.Right (first_state + 2)
                , newTransition "." "0" Program.None (success_state)]
                )
 
substitution_machine :: Symbol -> Symbol -> StateInt -> StateInt -> Machine
substitution_machine fromSymbol toSymbol success_state first_state =
  (first_state, 6) ==> collapse_machine toSymbol (first_state + 1) & \m ->
  (m, first_state + 1) ===> find_first Program.Left fromSymbol (first_state + 2) & \m ->
  (m, first_state + 2) ===> replace_first_until Program.Right "1" "B" [globalBlank, toSymbol] (first_state + 3) (first_state + 5) & \m ->
  (m, first_state + 3) ===> find_first Program.Right toSymbol (first_state + 4) & \m ->
  (m, first_state + 4) ===> shiftr_machine "Z" (first_state + 1) & \m ->
  (m, first_state + 5) ===> replace_all Program.Left "B" "1" [fromSymbol] success_state

step1 :: StateInt -> StateInt -> Machine
step1 success_state first_state =
  (first_state, 5) ==> copy_machine_rev "Y" "X" (first_state + 1) & \m ->
  (m, first_state + 1) ===> find_first Program.Left "X" (first_state + 2) & \m ->
  (m, first_state + 2) ===> replace_first Program.Right "0" "X" (first_state + 3) & \m ->
  (m, first_state + 3) ===> replace_first Program.Right "Y" "0" (first_state + 4) & \m ->
  (m, first_state + 4) ===> find_first Program.Right "Z" (success_state)

step2 :: StateInt -> StateInt -> Machine
step2 success_state first_state =
  copy_machine_rev "Z" "X" success_state first_state

step3 :: StateInt -> StateInt -> Machine
step3 success_state first_state =
  (first_state, 7) ==> replace_first Program.Left "X" "0" (first_state + 1) & \m ->
  (m, first_state + 1) ===> right_machine (first_state + 2) & \m ->
  (m, first_state + 2) ===> find_first Program.Right "0" (first_state + 3) & \m ->
  (m, first_state + 3) ===> find_first Program.Right "1" (first_state + 4) & \m ->
  (m, first_state + 4) ===> left_machine (first_state + 5) & \m ->
  (m, first_state + 5) ===> replace_first Program.Right "0" "Y" (first_state + 6) & \m ->
  (m, first_state + 6) ===> find_first Program.Left "X" success_state

shift_one_term_left :: Direction -> Symbol -> StateInt -> StateInt -> Machine
shift_one_term_left dir symbol success_state first_state =
  (first_state, 5) ==> find_first dir symbol (first_state + 1) & \m ->
  (m, first_state + 1) ===> replace_first Program.Left "0" symbol (first_state + 2) & \m ->
  (m, first_state + 2) ===> right_machine (first_state + 3) & \m ->
  (m, first_state + 3) ===> replace_first Program.Right symbol "0" (first_state + 4) & \m ->
  (m, first_state + 4) ===> find_first Program.Left symbol success_state

shift_one_term_right :: Direction -> Symbol -> StateInt -> StateInt -> Machine
shift_one_term_right dir symbol success_state first_state =
  (first_state, 10) ==> find_first dir symbol (first_state + 1) & \m ->
  (m, first_state + 1) ===> replace_first Program.Right "0" symbol (first_state + 2) & \m ->
  (m, first_state + 2) ===> find_first_until Program.Right "." ["0", "1"] (first_state + 6) (first_state + 9) & \m ->
  (m, first_state + 3) ===> left_machine (first_state + 4) & \m ->
  (m, first_state + 4) ===> replace_first Program.Left symbol "0" (first_state + 5) & \m ->
  (m, first_state + 5) ===> find_first Program.Right symbol success_state & \m ->
  (m, first_state + 6) ===> replace_first Program.Right "." "1" (first_state + 7) & \m ->
  (m, first_state + 7) ===> replace_first Program.Right "." "0" (first_state + 8) & \m ->
  (m, first_state + 8) ===> left_machine (first_state + 9) & \m ->
  (m, first_state + 9) ===> left_machine (first_state + 3)

compare_configuration :: StateInt -> StateInt -> StateInt -> Machine
compare_configuration success_state failed_state first_state =
  (first_state, 7) ==> matching_machine (first_state + 1) failed_state & \m -> -- ATTENTION (first_state + 5) & \m ->
  (m, first_state + 1) ===> replace_first Program.Right "0" "X" (first_state + 2) & \m ->
  (m, first_state + 2) ===> shift_one_term_right Program.Right "Y" (first_state + 3)  & \m ->
  (m, first_state + 3) ===> find_first Program.Left "X" (first_state + 4) & \m ->
  (m, first_state + 4) ===> matching_machine (first_state + 6) (first_state + 5) & \m ->
  (m, first_state + 5) ===> replace_first Program.Left "X" "0" failed_state & \m ->
  (m, first_state + 6) ===> replace_first Program.Left "X" "0" success_state

next_configuration :: StateInt -> StateInt -> Machine
next_configuration success_state first_state =
  (first_state, 8) ==> find_first Program.Right "Y" (first_state + 1) & \m ->
  (m, first_state + 1) ===> find_first Program.Right "0" (first_state + 2) & \m ->
  (m, first_state + 2) ===> right_machine (first_state + 3) & \m ->
  (m, first_state + 3) ===> machine_with_transition (
                [newTransition "0" "Y" Program.Right (first_state + 4)]
                ++ (not_letter ["0"] (newTransition "" "" Program.Right (first_state + 1)))
                ) & \m ->
  (m, first_state + 4) ===> find_first_until Program.Right "0" ["1"] failureState (first_state + 5) & \m ->
  (m, first_state + 5) ===> find_first Program.Left "Y" (first_state + 6) & \m ->
  (m, first_state + 6) ===> left_machine (first_state + 7) & \m ->
  (m, first_state + 7) ===> replace_first Program.Left "Y" "0" success_state

step4 :: StateInt -> StateInt -> Machine
step4 success_state first_state =
  (first_state, 5) ==> find_first Program.Left "X" (first_state + 1) & \m ->
  (m, first_state + 1) ===> compare_configuration success_state (first_state + 2) & \m ->
  (m, first_state + 2) ===> next_configuration (first_state + 3) & \m ->
  (m, first_state + 3) ===> find_first Program.Right "Y" (first_state + 4) & \m -> 
  (m, first_state + 4) ===> find_first_until Program.Right "Z" ["0", "1"] (failureState) (first_state)

step5 :: StateInt -> StateInt -> Machine
step5 success_state first_state =
  (first_state, 5) ==> right_machine (first_state + 1) & \m ->
  (m, first_state + 1) ===> find_first Program.Right "0" (first_state + 2) & \m ->
  (m, first_state + 2) ===> replace_all Program.Left "1" "0" ["X"] (first_state + 3) & \m ->
  (m, first_state + 3) ===> shift_one_term_right Program.Right "Y" (first_state + 4) & \m ->
  (m, first_state + 4) ===> shift_one_term_right Program.Right "Y" success_state

step6 :: StateInt -> StateInt -> Machine
step6 success_state first_state =
  (first_state, 2) ==> find_first Program.Right "Z" (first_state + 1) & \m ->
  (m, first_state + 1) ===> substitution_machine "Y" "Z" success_state

step7 :: StateInt -> StateInt -> Machine
step7 success_state first_state =
  (first_state, 6) ==> shift_one_term_right Program.Right "Y" (first_state + 1) & \m ->
  (m, first_state + 1) ===> find_first_until Program.Right "1" ["0"] (first_state + 2) failureState & \m ->
  (m, first_state + 2) ===> find_first_until Program.Right "1" ["0"] (first_state + 3) (first_state + 4) & \m ->
  (m, first_state + 3) ===> find_first_until Program.Right "1" ["0"] (first_state + 5) success_state & \m ->
  (m, first_state + 4) ===> shift_one_term_left Program.Left "Z" success_state & \m ->
  (m, first_state + 5) ===> shift_one_term_right Program.Right "Z" success_state

step8 :: StateInt -> StateInt -> Machine
step8 success_state first_state =
  (first_state, 2) ==> shift_one_term_left Program.Left "Y" (first_state + 1) & \m ->
  (m, first_state + 1) ===> shift_one_term_left Program.Left "Y" success_state


universal_machine :: StateInt -> StateInt -> Machine
universal_machine success_state first_state =
  (first_state, 9) ==> find_first Program.Right "Y" (first_state + 1) & \m ->
  (m, first_state + 1) ===> step1 (first_state + 2) & \m ->
  (m, first_state + 2) ===> step2 (first_state + 3) & \m ->
  (m, first_state + 3) ===> step3 (first_state + 4) & \m ->
  (m, first_state + 4) ===> step4 (first_state + 5) & \m ->
  (m, first_state + 5) ===> step5 (first_state + 6) & \m ->
  (m, first_state + 6) ===> step6 (first_state + 7) & \m ->
  (m, first_state + 7) ===> step7 (first_state + 8) & \m ->
  (m, first_state + 8) ===> step8 first_state

universal = 
--    let trans = find_first_until Program.Right "X" [] 0 1 2 in
--    let trans = replace_first_until Program.Right "X" "B" [] 0 1 2 in
--    let trans = replace_all Program.Right "X" "B" ["Z"] 0 1 in
--    let trans = copy_machine "X" "Z" 0 1 in
--    let trans = copy_machine_rev "Z" "X" 0 1 in
--    let trans = matching_machine 0 1 2 in
--    let trans = collapse_machine 0 1 in
--    let trans = shiftr_machine 0 1 in
--    let trans = substitution_machine 0 1 in
   let trans = universal_machine 0 1 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet ++ ["."]
        , blank = "."
        , states = List.map (\x -> show x) [-1..(maxState trans + 1)]
        , initial = "1"
        , finals = []
        , transitions = trans
    }
