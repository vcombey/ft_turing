{-# LANGUAGE DeriveGeneric #-}
module Generate
  (
    find
  , erase
  , erase_all
  , universal
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

globalAlphabet = ["e", "0", "1", "."]
globalBlank = "."

not_letter :: [Symbol] -> Transition -> [Transition]
not_letter list_not_symb trans =
  List.map (\symb -> if Program.write trans /= "" then trans { Program.read = symb } else trans { Program.read = symb, Program.write = symb }) (globalAlphabet List.\\ list_not_symb)

any_letter = not_letter []

newTransition :: Symbol -> Symbol -> Direction -> StateInt -> Transition
newTransition read write action state =
  Transition {
    Program.read = read
  , Program.write = write
  , to_state = show state
  , action = action
  }

find :: StateInt -> StateInt -> Symbol -> StateInt -> Machine
find c b alpha first_state =
  let nb_state = 3 in
  HashMap.empty &
          HashMap.insert (show first_state)
                    ([newTransition "e" "e" Program.Left (first_state + 1)]
                    ++ (not_letter ["e"] (newTransition "" "" Program.Left first_state)))
               & 
          HashMap.insert (show (first_state + 1))
                    ([(newTransition alpha alpha None c),
                     newTransition globalBlank alpha Program.Right (first_state + 2)]
                    ++ (not_letter [alpha, globalBlank] (newTransition "" "" Program.Right (first_state + 1))))
               &
          HashMap.insert (show (first_state + 2))
                    ([newTransition alpha alpha None c ,
                     newTransition globalBlank globalBlank Program.Right b]
                    ++ (not_letter [alpha, globalBlank] (newTransition "" "" Program.Right (first_state + 1) )))

erase :: StateInt -> StateInt -> Symbol -> StateInt -> Machine
erase c b alpha first_state = 
  let nb_state = 1 in
    find first_state b alpha (first_state + nb_state) &
    HashMap.insert (show first_state)
                    (any_letter (newTransition "" "" None c))

erase_all :: StateInt -> Symbol -> StateInt -> Machine
erase_all b alpha first_state = 
  erase first_state b alpha first_state

universal = 
  let trans = erase_all 0 "0" 1 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet
        , blank = globalBlank
        , states = List.map (\x -> show x) [0..(HashMap.size trans + 1)]
        , initial = "0"
        , finals = []
        , transitions = trans
    }
