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

--find :: StateInt -> StateInt -> Symbol -> StateInt -> Machine
--find c b alpha first_state =
--  let nb_state = 3 in
--  HashMap.empty &
--          HashMap.insert (show first_state)
--                    ([newTransition "e" "e" Program.Left (first_state + 1)]
--                    ++ (not_letter ["e"] (newTransition "" "" Program.Left first_state)))
--               & 
--          HashMap.insert (show (first_state + 1))
--                    ([(newTransition alpha alpha None c),
--                     newTransition globalBlank alpha Program.Right (first_state + 2)]
--                    ++ (not_letter [alpha, globalBlank] (newTransition "" "" Program.Right (first_state + 1))))
--               &
--          HashMap.insert (show (first_state + 2))
--                    ([newTransition alpha alpha None c ,
--                     newTransition globalBlank globalBlank Program.Right b]
--                    ++ (not_letter [alpha, globalBlank] (newTransition "" "" Program.Right (first_state + 1) )))

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
find_first_until dir alpha until found_state not_found_state first_state =
    HashMap.empty &
    HashMap.insert (show (first_state))
            ([(newTransition alpha alpha None found_state)]
            ++ (for_letter until (newTransition "" "" Program.None not_found_state))
            ++ not_letter (until ++ [alpha]) (newTransition "" "" dir (first_state)))

find_first :: Direction -> Symbol -> StateInt -> StateInt -> Machine
find_first dir alpha found_state first_state =
  find_first_until dir alpha [] found_state found_state first_state

replace_first_until :: Direction -> Symbol -> Symbol -> [Symbol] -> StateInt -> StateInt -> StateInt -> Machine
replace_first_until dir alpha beta until found_state not_found_state first_state =
    (first_state, 2) ==> find_first_until dir alpha until (first_state + 1) not_found_state &
    HashMap.insert (show (first_state + 1))
            ([(newTransition alpha beta None found_state)]
            ++ not_letter ([alpha]) (newTransition "" "" dir (not_found_state)))

replace_first :: Direction -> Symbol -> Symbol -> StateInt -> StateInt -> Machine
replace_first dir alpha beta found_state first_state =
    replace_first_until dir alpha beta [] found_state 0 first_state

replace_all :: Direction -> Symbol -> Symbol -> [Symbol] -> StateInt -> StateInt -> Machine
replace_all dir alpha beta until found_state first_state =
  replace_first_until dir alpha beta until first_state found_state (first_state ) -- MARCHE PA

type FunctionMachine = (StateInt -> StateInt -> StateInt -> Machine)

compose_function :: FunctionMachine -> FunctionMachine -> StateInt -> StateInt -> StateInt -> Machine
compose_function f1 f2 global_success global_failure first_state = 
     (first_state, 2) ==> f1 (first_state + 1) global_failure & \m ->
     (m, first_state + 1) ===> f2 (global_success) (global_failure)
    
copy_machine :: Direction -> StateInt -> StateInt -> Machine
copy_machine dir found_state first_state =
  -- (((first_state, 4) ==> replace_first_until dir "1" "B" ["Y", globalBlank] (first_state + 1) (first_state + 3)
  -- ), first_state + 1)
  -- ===> find_first dir "Y" (first_state + 2)

  ((first_state, 4) ==> replace_first_until dir "1" "B" ["Y", globalBlank] (first_state + 1) (first_state + 3)) & \m ->
  (m, first_state + 1) ===> compose_function (find_first_until dir "Y" []) (replace_first_until dir "0" "1" []) (first_state + 2) 0 & \m ->
  (m, first_state + 2) ===> find_first (Program.reverseDir dir) "B" (first_state) & \n ->
  (m, first_state + 3) ===> replace_all (Program.reverseDir dir) "B" "1" ["X"] found_state

   -- union (replace_first_until dir "0" "1" [] (first_state + 1) (first_state + 3) first_state) &
    


--erase :: StateInt -> StateInt -> Symbol -> StateInt -> Machine
--erase c b alpha first_state = 
--  let nb_state = 1 in
--    find first_state b alpha (first_state + nb_state) &
--    HashMap.insert (show first_state)
--                    (any_letter (newTransition "" globalBlank None c))
--
--erase_all :: StateInt -> Symbol -> StateInt -> Machine
--erase_all b alpha first_state = 
--  erase first_state b alpha first_state

--copying_machine :: StateInt -> StateInt -> Machine
--copying_machine first_state final_state =
--  let nb_state = 1 in
--    transition_state first_state (first_state + nb_state) &
--
--    find first_state b "1" (first_state + 1) &

universal = 
--  let trans = find_first_until Program.Right "X" [] 0 1 2 in
--    let trans = replace_first_until Program.Right "X" "B" [] 0 1 2 in
--    let trans = replace_all Program.Right "X" "B" ["Z"] 0 1 in
    let trans = copy_machine Program.Right 0 1 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet ++ ["."]
        , blank = "."
        , states = List.map (\x -> show x) [0..(maxState trans + 1)]
        , initial = "1"
        , finals = []
        , transitions = trans
    }
