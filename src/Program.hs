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
  , prettyProgram
  , prettyTransition
  , reverseDir
  , transpileProgram
  )
  where 

import GHC.Generics
import Data.Aeson
import qualified Data.List as List
import Data.Text  (unpack, pack)
import Data.HashMap.Strict as HashMap

data Direction = Left | Right | None deriving (Show, Generic)
instance ToJSON Direction where
   toJSON (dir) = String (case dir of
     Program.Left -> pack "LEFT"
     Program.Right -> pack "RIGHT"
     Program.None -> pack "NONE")

reverseDir dir = case dir of
     Program.Left -> Program.Right
     Program.Right -> Program.Left
     Program.None -> Program.None

transpileDirection dir = case dir of
     Program.Left -> "1"
     Program.None -> "11"
     Program.Right -> "111"

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \s -> case unpack s of
    "LEFT" -> return Program.Left
    "RIGHT" -> return Program.Right
    "NONE" -> return Program.None
  
type State = String

transpileState state_to_code s = 
  case List.lookup s state_to_code of
  Just res -> res
  Nothing -> error "Maybe.fromJust: Nothing" 

type Symbol = String

transpileSymbol symb_to_code sym = 
  case List.lookup sym symb_to_code of
  Just res -> res
  Nothing -> error "Maybe.fromJust: Nothing" 

data Transition = Transition {
  read :: Symbol
  , to_state :: State
  , write :: Symbol
  , action :: Direction
} deriving (Show, Generic)
instance FromJSON Transition
instance ToJSON Transition

transpileTransition :: State -> Transition -> (Symbol -> String) -> (State -> String) -> String
transpileTransition state trans transSym transState =
  transState state ++
  "0" ++
  transSym (Program.read trans) ++
  "0" ++
  transState (Program.to_state trans) ++
  "0" ++
  transSym (Program.write trans) ++
  "0" ++
  transpileDirection (Program.action trans) ++
  "00" 
  
prettyTransition state t =
  "(" ++ state ++ ", " ++ (Program.read t) ++ ") -> (" ++ (to_state t) ++ ", " ++ (write t) ++ ", " ++ (show (action t)) ++ ")"
 
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
instance ToJSON Program

prettyProgram p =
        divider                           ++ "\n" ++
        divider'                          ++ "\n" ++
        prettyName (name p)               ++ "\n" ++
        divider'                          ++ "\n" ++
        divider                           ++ "\n" ++
        prettyAlphabet (alphabet p)       ++ "\n" ++
        prettyStates (states p)           ++ "\n" ++
        prettyInitial (initial p)         ++ "\n" ++
        prettyFinals (finals p)           ++ 
        prettyTransitions (transitions p) ++
        divider
        where
                divider  = "*******************************************************************************"
                divider' = "*                                                                             *"
                prettyName name = (
                  let l_name = length name in
                  let l_line = length divider in
                  let l_diff = (l_line - l_name - 2) in
                  let l_diff_pair = l_diff `mod` 2 == 0 in
                  let l_void = (l_diff `div` 2) in
                  let void = replicate l_void ' ' in
                  let add = if l_diff_pair then "" else " " in
                  "*" ++ void ++ name ++ void ++ add ++ "*")
                prettyAlphabet a = "Alphabet : " ++ show a
                prettyStates s = "State : " ++ show s
                prettyInitial i = "Initial : " ++ show i
                prettyFinals f = "Finals : " ++ show f
                prettyTransitions t =
                  let concat_strings strings = List.foldl' (\acc b -> acc ++ "\n" ++ b) "" strings in
                    (HashMap.foldlWithKey' (\acc key list_transition -> acc ++ (concat_strings [prettyTransition key x | x <- list_transition])) "" t ++ "\n")


--Check that the program is well formed
check :: Program -> Either String ()
check t =
    if not (belongToStates (initial t)) then Prelude.Left ("Initial don't belong to states")
    else if not (List.all (\x -> length x == 1) (alphabet t)) then Prelude.Left ("All Symbol are not Characters")
    else if not (List.all belongToStates (finals t)) then Prelude.Left ("All finals don't belong to states")
    else if not (belongToAlphabet (blank t)) then Prelude.Left ("blank dont belong to alphabet")
    else
      case (List.find (not . checkTransition) (toList $ transitions t)) of
        Just invalid_trans -> Prelude.Left ("Transition not valable" ++ (show invalid_trans))
        Nothing -> Prelude.Right ()
    where belongToStates s = elem s (states t)
          belongToAlphabet s = elem s (alphabet t)
          checkTransition (n, trans) = belongToStates n
                                        && List.all (belongToStates . to_state) trans
                                        && List.all (belongToAlphabet . Program.read) trans
                                        && List.all (belongToAlphabet . Program.write) trans

--From a program a state a a letter read in the tape get the corresponding transition
getTransition :: Program -> State -> Symbol -> Maybe Transition
getTransition p state symbol = HashMap.lookup state (transitions p)
  >>= \list_transition -> List.find (\x -> Program.read x == symbol) list_transition


transpileProgram :: Program -> String -> String
transpileProgram p input =
  let new_alphabet = (blank p) : (List.filter (blank p /= ) (alphabet p)) in
  let symb_to_code = List.map (\(s, i) -> (s, replicate i '1')) (new_alphabet `List.zip` [1..]) in
  let new_states = (initial p) : (List.filter (initial p /= ) (states p)) in
  let states_to_code = List.map (\(s, i) -> (s, replicate i '1')) (new_states `List.zip` [1..]) in
  let state_index state = List.find (\(s, i) -> s == state) states_to_code in
  let concat_strings strings = List.foldl' (\acc b -> acc ++ b) "" strings in
  let l = List.map (\(key, list_transition) -> (key, (concat_strings [transpileTransition key t (transpileSymbol symb_to_code) (transpileState states_to_code) | t <- list_transition]))) (HashMap.toList $ transitions p) in
 -- in show l
  let sorted = List.sortOn (\(key,_) -> case state_index key of
                                           Just (_, i) -> i
                                           Nothing -> error "blabla") l in
  "X" ++ replicate ((List.length $ alphabet p) + (List.length $ states p) + 2) '0' ++ 
  "Y" ++ (concat $ List.map (\(_, s) -> s) sorted)
  ++ "Z" ++ (List.concat $ (List.map (\c -> transpileSymbol symb_to_code [c] ++ "0") input))

  
