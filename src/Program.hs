{-# LANGUAGE DeriveGeneric #-}
module Program
  (
    Program
  , Direction (..)
  , Transition
  , State
 {-- , new
  , check --}
  )
  where 

import GHC.Generics
import Data.Aeson
import Data.List

data Direction = Left | Right deriving (Show, Generic)
instance FromJSON Direction

type State = String

data Transition = Transition {
  read :: Char
  , toState :: String
  , write :: Char
  , action :: Direction
} deriving (Show, Generic)
instance FromJSON Transition

data Program = Program {
    name :: String
    , alphabet :: [Char]
   {--  , blank :: Char
    , states :: [State]
    , initial :: State
    , finals :: [State]
   , transitions :: [(State, [Transition])] --}
} deriving (Show, Generic)
instance FromJSON Program
{--
new :: Program
new = Program {
        name = "unary_sub"
        , alphabet = [ '1', '.', '-', '=' ]
        , blank = '.'
        , states = [ "scanright", "eraseone", "subone", "skip", "HALT" ]
        , initial = "scanright"
        , finals = ["HALT"]
        , transitions = [
            ("scanright", [
                Transition { Program.read= '.', toState= "scanright", write= '.', action= Program.Right},
                Transition { Program.read= '1', toState= "scanright", write= '1', action= Program.Right},
                Transition { Program.read= '-', toState= "scanright", write= '-', action= Program.Right},
                Transition { Program.read= '=', toState= "eraseone" , write= '.', action= Program.Left }
            ]),
            ("eraseone", [
                Transition { Program.read= '1', toState= "subone", write= '=', action= Program.Left},
                Transition { Program.read= '-', toState= "HALT"  , write= '.', action= Program.Left}
            ]),
            ("subone", [
                Transition { Program.read= '1', toState= "subone", write= '1', action= Program.Left},
                Transition { Program.read= '-', toState= "skip"  , write= '-', action= Program.Left}
            ]),
            ("skip", [
                Transition { Program.read= '.', toState= "skip" , write= '.', action= Program.Left},
                Transition { Program.read= '1', toState= "scanright", write= '.', action= Program.Right}
            ])
            ]
        }

--Check that the program is well formed
check :: Program -> Bool
check t =
    belongToStates (initial t)
    && all belongToStates (finals t)
    && belongToAlphabet (blank t)
    && all (checkTransition) (transitions t)
    where belongToStates s = elem s (states t)
          belongToAlphabet s = elem s (alphabet t)
          checkTransition (n, trans) = belongToStates n
                                        && all (belongToStates . toState) trans
                                        && all (belongToAlphabet . Program.read) trans
                                        && all (belongToAlphabet . Program.write) trans

--From a program a state a a letter read in the tape get the corresponding transition
getTransition :: Program -> State -> Char -> Maybe Transition
getTransition p state symbol = find (\(s, _) -> s == state) (transitions p)
  >>= \(_, list_transition) -> find (\x -> Program.read x == symbol) list_transition

--}
