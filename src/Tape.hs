module Tape
  (
    Cell
  , Tape
  , readSymbol
  , putSymbol
  , fromString
  )
  where 

import qualified Data.Map as IntMap
import Program

type Cell = Int
type Tape = IntMap.Map Cell Symbol

readSymbol :: Cell -> Symbol -> Tape -> Symbol
readSymbol cell blank tape = case IntMap.lookup cell tape of
  Nothing -> blank
  Just s -> s

putSymbol :: Cell -> Symbol -> Tape -> Tape
putSymbol cell symbol tape = IntMap.insert cell symbol tape

fromString :: String -> Tape
fromString s = IntMap.fromList $ zip [0..] (map (\x -> [x]) s)
