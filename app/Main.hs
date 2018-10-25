module Main where

import qualified Data.Map as Map
import qualified Data.List.Split as Split
import qualified Program 
import qualified Machine 
import qualified Tape 
import Data.Aeson
import System.Environment
import System.Exit
import Generate

universal = encodeFile "universal_turing_machine.json" Generate.universal

check_program_input file input = ((decodeFileStrict file) :: IO (Maybe Program.Program)) >>=
       \decoded -> case decoded of
         Just program -> 
           let tape = Tape.fromString input in
           case (Program.check program) of
             Left (mess) -> (putStrLn (Program.prettyProgram program) >> putStrLn ("bad program: " ++ mess)) >> return Nothing
             Right () -> if not (Tape.check tape program) then putStrLn "all char in initial tape don't belongs to (alphabet \\ blank)" >> return Nothing else return (Just (program, tape))
         Nothing -> putStrLn "parsing error" >> return (Nothing)

encodeInputUTM file input  = ((decodeFileStrict file) :: IO (Maybe Program.Program)) >>=
       \decoded -> case decoded of
         Just program -> putStrLn $ show (Program.transpileProgram program input)
         Nothing -> putStrLn "parsing error"

decodeOutputUTM last = do  
    line <- getLine
    if line !! 0 == 'T' then
        let split1 = Split.splitOn "Z" last in
        let tape = split1 !! 1 in
        let split2 = Split.splitOn "<" tape in
        let output = split2 !! 0 in
        putStrLn output
    else
        Main.decodeOutputUTM line
    

start file input = ((decodeFileStrict file) :: IO (Maybe Program.Program)) >>=
       \decoded -> case decoded of
         Just program -> 
           let tape = Tape.fromString input in
           case (Program.check program) of
             Left (mess) -> (putStrLn (Program.prettyProgram program) >> putStrLn ("bad program: " ++ mess))
             Right () -> if not (Tape.check tape program) then putStrLn "all char in initial tape don't belongs to (alphabet \\ blank)" else putStrLn (Program.prettyProgram program) >> Machine.execute tape program
         Nothing -> putStrLn "parsing error"

main :: IO ()
main = getArgs >>= parse

 
parse x | x == ["-h"] || x == ["--help"] = usage >> exitWith ExitSuccess
parse x | x == ["-u"] || x == ["--universal"] = Main.universal
parse (x:file:input:[]) | x == "-e" || x == "--encode" = encodeInputUTM file input
parse x | x == ["-d"] || x == ["--decode"] = Main.decodeOutputUTM "start"

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

