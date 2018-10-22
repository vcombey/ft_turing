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


find_first :: Symbol -> [Symbol] -> StateInt -> StateInt -> StateInt -> Machine
find_first alpha until found_state not_found_state first_state =
    HashMap.empty &
    HashMap.insert (show (first_state))
            ([(newTransition alpha alpha None found_state)]
            ++ (for_letter until (newTransition "" "" Program.None not_found_state))
            ++ not_letter (until ++ [alpha]) (newTransition "" "" Program.Right (first_state)))

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
  let trans = find_first "0" [] 0 1 2 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet
        , blank = globalBlank
        , states = List.map (\x -> show x) [0..(HashMap.size trans + 1)]
        , initial = "2"
        , finals = []
        , transitions = trans
    }
