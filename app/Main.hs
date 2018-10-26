module Main where

import qualified Data.Map as Map
import qualified Data.List.Split as Split
import qualified Program 
import qualified Machine 
import qualified Tape 
import qualified Generate 
import Data.Aeson
import System.Environment
import System.Exit
import Generate

universal = encodeFile "universal_turing_machine.json" Generate.universal

parse_input file input on_success = ((eitherDecodeFileStrict file) :: IO (Either String Program.Program)) >>=
       \decoded -> case decoded of
         Right program -> 
           let tape = Tape.fromString input in
           case (Program.check program) of
             Left mess -> (putStrLn (Program.prettyProgram program) >> putStrLn ("bad program: " ++ mess))
             Right () -> if not (Tape.check tape program)
                           then putStrLn "all char in initial tape don't belongs to (alphabet \\ blank)"
                           else on_success program tape
         Left mess -> putStrLn mess

encodeInputUTM file input  = 
    parse_input file input (\program tape -> putStrLn $ show (Program.transpileProgram program input))

decodeOutputUTM file last_line = do  
    line <- getLine
    if line !! 0 == 'T' then
        let split1 = Split.splitOn "Z" last_line in
        let tape = split1 !! 1 in
        let split2 = Split.splitOn "." tape in
        let output = split2 !! 0 in
        parse_input file "" (\program tape -> putStrLn $ (line ++ "\n" ++ Program.decodeTape program output))
    else
        putStrLn line >> Main.decodeOutputUTM file line
    
start file input =
    parse_input file input (\program tape -> putStrLn (Program.prettyProgram program) >> Machine.execute tape program)

main :: IO ()
main = getArgs >>= parse

 
parse x | x == ["-h"] || x == ["--help"] = usage >> exitWith ExitSuccess
parse x | x == ["-u"] || x == ["--universal"] = Main.universal
parse (x:file:input:[]) | x == "-e" || x == "--encode" = encodeInputUTM file input
parse (x:file:[]) | x == "-d" || x == "--decode" = Main.decodeOutputUTM file ""

parse []     = usage >> exitWith (ExitFailure 1)
parse (_:[]) = usage >> exitWith (ExitFailure 1)

parse (file:input:[]) = start file input

parse _     = usage >> exitWith (ExitFailure 1)

usage = putStrLn "Usage ft_turing [-h | -u | -e | -d] jsonfile input\n\n\
                    \positional arguments:\n\
                    \  jsonfile              json description of the machine\n\
                    \  input                 input of the machine\n\n\
                    \optional arguments:\n\
                    \  -h, --help            show this help message and exit\n\
                    \  -u, --universal       generate universal turing machine\n\
                    \  -e, --encode          encode the input of the univeral turing machine\n\
                    \  -d, --decode          decode the output of the universal turing machine\n"

