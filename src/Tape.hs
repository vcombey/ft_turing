module Tape
  (
    Cell
  , Tape
  , readSymbol
  , putSymbol
  )
  where 

import qualified Data.Map as IntMap

type Cell = Int
type Tape = IntMap.Map Int Char

readSymbol :: Cell -> Tape -> Maybe Char
readSymbol cell tape = IntMap.lookup cell tape

putSymbol :: Cell -> Char -> Tape -> Tape
putSymbol cell symbol tape = IntMap.insert cell symbol tape

{--
fromInitialState :: String -> Tape
fromInitialState = fold (\acc \symb \i -> putSymbol i symb acc) IntMap.empty
--}

