
data Direction = Left | Right deriving Show

data Transition = Transition {
  read :: Char
  , toState :: String
  , write :: Char
  , action :: Direction
} deriving Show

data TuringMachine = TuringMachine {
    name :: String
    , alphabet :: [Char]
    , blank :: Char
    , states :: [String]
    , initial :: String
    , finals :: String
    , transitions :: [(String, [Transition])]
} deriving Show

new :: TuringMachine
new = TuringMachine {
        name = "unary_sub"
        , alphabet = [ '1', '.', '-', '=' ]
        , blank = '.'
        , states = [ "scanright", "eraseone", "subone", "skip", "HALT" ]
        , initial = "scanright"
        , finals = "HALT"
        , transitions = [
            ("scanright", [
                Transition { Machine.read= '.', toState= "scanright", write= '.', action= Machine.Right},
                Transition { Machine.read= '1', toState= "scanright", write= '1', action= Machine.Right},
                Transition { Machine.read= '-', toState= "scanright", write= '-', action= Machine.Right},
                Transition { Machine.read= '=', toState= "eraseone" , write= '.', action= Machine.Left }
            ]),
            ("eraseone", [
                Transition { Machine.read= '1', toState= "subone", write= '=', action= Machine.Left},
                Transition { Machine.read= '-', toState= "HALT"  , write= '.', action= Machine.Left}
            ]),
            ("subone", [
                Transition { Machine.read= '1', toState= "subone", write= '1', action= Machine.Left},
                Transition { Machine.read= '-', toState= "skip"  , write= '-', action= Machine.Left}
            ]),
            ("skip", [
                Transition { Machine.read= '.', toState= "skip" , write= '.', action= Machine.Left},
                Transition { Machine.read= '1', toState= "scanright", write= '.', action= Machine.Right}
            ])
            ]
        }

check :: TuringMachine -> Bool
check t =
    belongToStates (initial t)
    && belongToStates (finals t)
    && belongToAlphabet (blank t)
    && all (checkTransition) (transitions t)
    where belongToStates s = elem s (states t)
          belongToAlphabet s = elem s (alphabet t)
          checkTransition (n, trans) = belongToStates n
                                        && all (belongToStates . toState) trans
                                        && all (belongToAlphabet . Machine.read) trans
                                        && all (belongToAlphabet . Machine.write) trans

getTransition :: Program -> State -> Maybe Transition
getTransition p s = find (\x -> x == s)  (transitions p)
  >>= (\(_, list_transition) -> find (\x -> x
