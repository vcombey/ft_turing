module Main where

import qualified Data.Map as Map
import Program 
import Machine 
import Tape 
import Data.Aeson
import System.Environment
import System.Exit

start file input = ((decodeFileStrict file) :: IO (Maybe Program)) >>=
       \decoded -> case decoded of
         Just program -> putStrLn (prettyProgram program) >> putStrLn (show $ Program.check program) >>
           putStrLn (prettyOutput (Machine.execute (Tape.fromString input) program) program)
         Nothing -> putStrLn "parsing error"

main :: IO ()
main = getArgs >>= parse
 
parse x | x == ["-h"] || x == ["--help"] = usage >> exitWith ExitSuccess
parse []     = usage >> exitWith (ExitFailure 1)
parse (_:[]) = usage >> exitWith (ExitFailure 1)
parse (file:input:[]) = start file input

usage = putStrLn "Usage ft_turing [-h] jsonfile input\n\
                    \positional arguments:\n\
                    \jsonfile              json description of the machine\n\
                    \input                 input of the machine\n\
                    \optional arguments:\n\
                    \-h, --help            show this help message and exit\n"

