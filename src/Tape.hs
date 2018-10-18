
data Tape = IntMap Char

readSymbol :: Key -> Tape -> Tape
readSymbol num ruban = IntMap.lookup num ruban

putSymbol :: Key -> a -> Tape -> Tape
putSymbol num symbol ruban = IntMap.insert num symbol ruban

fromInitialState :: String -> Tape
fromInitialState = fold (\acc \symb \i -> putSymbol i symb acc) IntMap.empty

