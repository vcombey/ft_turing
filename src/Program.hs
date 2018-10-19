{-# LANGUAGE DeriveGeneric #-}
module Program
  (
    Program (..)
  , Direction (..)
  , Transition (..)
  , State
  , check
  , Symbol
  , getTransition
  )
  where 

import GHC.Generics
import Data.Aeson
import Data.List
import Data.Text  (unpack)
import Data.HashMap.Strict as HashMap

data Direction = Left | Right deriving (Show, Generic)

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \s -> case unpack s of
    "LEFT" -> return Program.Left
    "RIGHT" -> return Program.Right
  
type State = String
type Symbol = String

data Transition = Transition {
  read :: Symbol
  , to_state :: State
  , write :: Symbol
  , action :: Direction
} deriving (Show, Generic)
instance FromJSON Transition

prettyTransition state t =
  "(" : state : ", " : (read t) : ") -> (" : (to_state t) : ", " : (write t) : (show (action t))
 
data Program = Program {
    name :: String
    , alphabet :: [Symbol]
    , blank :: Symbol
    , states :: [State]
    , initial :: State
    , finals :: [State]
   , transitions :: HashMap State [Transition]
} deriving (Show, Generic)
instance FromJSON Program

centerName n =
  let l_name = length n in
  let l_line = length "********************************************************************************" in
  

prettyProgram p =
  "********************************************************************************\n\
  \*                                                                              *\n\
  \*                                     unary_sub                     *\n\
** ********************************************************************************

--Check that the program is well formed
check :: Program -> Bool
check t =
    belongToStates (initial t)
    && all (\x -> length x == 1) (alphabet t)
    && all belongToStates (finals t)
    && belongToAlphabet (blank t)
    && all (checkTransition) (toList $ transitions t)
    where belongToStates s = elem s (states t)
          belongToAlphabet s = elem s (alphabet t)
          checkTransition (n, trans) = belongToStates n
                                        && all (belongToStates . to_state) trans
                                        && all (belongToAlphabet . Program.read) trans
                                        && all (belongToAlphabet . Program.write) trans

--From a program a state a a letter read in the tape get the corresponding transition
getTransition :: Program -> State -> Symbol -> Maybe Transition
getTransition p state symbol = HashMap.lookup state (transitions p)
  >>= \list_transition -> find (\x -> Program.read x == symbol) list_transition

