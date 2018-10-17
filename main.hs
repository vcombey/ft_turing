data Direction = Left | Right deriving Show

data Transition = Transition {
  read :: Char
  , to_state :: String
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

check_turing_machine :: TuringMachine -> Bool
check_turing_machine t =
    belong_to_states (initial t)
    && belong_to_states (finals t)
    && belong_to_alphabet (blank t)
    && all (check_transition) (transitions t)
    where belong_to_states s = elem s (states t)
          belong_to_alphabet s = elem s (alphabet t)
          check_transition (n, trans) = belong_to_states n
                                        && all (belong_to_states . to_state) trans
                                        && all (belong_to_alphabet . Main.read) trans
                                        && all (belong_to_alphabet . Main.write) trans

main :: IO ()
main = putStrLn (show t)
       >> putStrLn (show $ check_turing_machine t)
    where t = TuringMachine {
        name = "unary_sub"
        , alphabet = [ '1', '.', '-', '=' ]
        , blank = '.'
        , states = [ "scanright", "eraseone", "subone", "skip", "HALT" ]
        , initial = "scanright"
        , finals = "HALT"
        , transitions = [
            ("scanright", [
                Transition { Main.read= '.', to_state= "scanright", write= '.', action= Main.Right},
                Transition { Main.read= '1', to_state= "scanright", write= '1', action= Main.Right},
                Transition { Main.read= '-', to_state= "scanright", write= '-', action= Main.Right},
                Transition { Main.read= '=', to_state= "eraseone" , write= '.', action= Main.Left }
            ]),
            ("eraseone", [
                Transition { Main.read= '1', to_state= "subone", write= '=', action= Main.Left},
                Transition { Main.read= '-', to_state= "HALT"  , write= '.', action= Main.Left}
            ]),
            ("subone", [
                Transition { Main.read= '1', to_state= "subone", write= '1', action= Main.Left},
                Transition { Main.read= '-', to_state= "skip"  , write= '-', action= Main.Left}
            ]),
            ("skip", [
                Transition { Main.read= '.', to_state= "skip" , write= '.', action= Main.Left},
                Transition { Main.read= '1', to_state= "scanright", write= '.', action= Main.Right}
            ])
            ]
        }
