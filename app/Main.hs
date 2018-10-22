module Main where

import qualified Data.Map as Map
import qualified Program 
import qualified Machine 
import qualified Tape 
import qualified Generate 
import Data.Aeson
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as ByteString

universal = encodeFile "universal_turing_machine" Generate.universal

start file input = ((decodeFileStrict file) :: IO (Maybe Program.Program)) >>=
       \decoded -> case decoded of
         Just program -> 
           let tape = Tape.fromString input in
           if not (Program.check program) then putStrLn "bad program"
           else if not (Tape.check tape program) then putStrLn "all char in initial tape don't belongs to (alphabet \\ blank)"
           else putStrLn (Program.prettyProgram program) >> Machine.execute tape program
         Nothing -> putStrLn "parsing error"

main :: IO ()
main = getArgs >>= parse
 
parse x | x == ["-h"] || x == ["--help"] = usage >> exitWith ExitSuccess
parse x | x == ["-u"] || x == ["--universal"] = universal
parse []     = usage >> exitWith (ExitFailure 1)
parse (_:[]) = usage >> exitWith (ExitFailure 1)
parse (file:input:[]) = start file input
parse _     = usage >> exitWith (ExitFailure 1)

usage = putStrLn "Usage ft_turing [-h] jsonfile input\n\n\
                    \positional arguments:\n\
                    \jsonfile              json description of the machine\n\
                    \input                 input of the machine\n\n\
                    \optional arguments:\n\
                    \-h, --help            show this help message and exit\n"

