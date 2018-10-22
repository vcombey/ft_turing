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

type Symbol = String
type State = Int
data Direction = Left | Right | None deriving (Show, Generic)
instance ToJSON Direction where

data Program = Program {
    name :: String
    , alphabet :: [Symbol]
    , blank :: Symbol
    , states :: [State]
    , initial :: State
    , finals :: [State]
   , transitions :: HashMap State [Transition]
} deriving (Show, Generic)
instance ToJSON Program

data Transition = Transition {
  read :: Symbol
  , to_state :: State
  , write :: Symbol
  , action :: Direction
} deriving (Show, Generic)
instance ToJSON Transition where

type Machine = HashMap State [Transition]


globalAlphabet = ["e", "0", "1", "."]
globalBlank = "."

not_letter :: [Symbol] -> Transition -> [Transition]
not_letter list_not_symb trans =
  List.map (\symb -> if Generate.write trans /= "" then trans { Generate.read = symb } else trans { Generate.read = symb, Generate.write = symb }) (globalAlphabet List.\\ list_not_symb)

any_letter = not_letter []
  
find :: State -> State -> Symbol -> State -> Machine
find c b alpha first_state =
  let nb_state = 3 in
  HashMap.empty &
          HashMap.insert first_state
                    ([Transition { Generate.read = "e", Generate.write = "e", action = Generate.Left, to_state = first_state + 1 }]
                    ++ (not_letter ["e"] (Transition { Generate.read = "", Generate.write = "", action = Generate.Left, to_state = first_state })))
               & 
          HashMap.insert (first_state + 1)
                    ([Transition { Generate.read = alpha , Generate.write = alpha, action = None, to_state = c },
                     Transition { Generate.read = globalBlank , Generate.write = alpha, action = Generate.Right, to_state = first_state + 2 }]
                    ++ (not_letter [alpha, globalBlank] (Transition { Generate.read = "", Generate.write = "", action = Generate.Right, to_state = first_state + 1 })))
               &
          HashMap.insert (first_state + 2)
                    ([Transition { Generate.read = alpha , Generate.write = alpha, action = None, to_state = c },
                     Transition { Generate.read = globalBlank , Generate.write = globalBlank, action = Generate.Right, to_state = b }]
                    ++ (not_letter [alpha, globalBlank] (Transition { Generate.read = "", Generate.write = "", action = Generate.Right, to_state = first_state + 1 })))

erase :: State -> State -> Symbol -> State -> Machine
erase c b alpha first_state = 
  let nb_state = 1 in
  let machine = HashMap.empty in
    let machine = find first_state b alpha (first_state + nb_state) in

    let machine = HashMap.insert first_state
                    (any_letter (Transition { Generate.read = "", Generate.write = "", action = None, to_state = c}))
                machine in
    machine

erase_all :: State -> Symbol -> State -> Machine
erase_all b alpha first_state = 
  erase first_state b alpha first_state

universal = 
  let trans = find 0 1 "0" 2 in
   Program {
        name="Turing'ception"
        , alphabet = globalAlphabet
        , blank = globalBlank
        , states = [0..(HashMap.size trans + 1)]
        , initial = 0
        , finals = []
        , transitions = trans
    }
