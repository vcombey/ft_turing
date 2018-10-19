module Main where

import qualified Data.Map as Map
import Program 
import Machine 
import Tape 
import Data.Aeson

main :: IO ()
main = ((decodeFileStrict "ressources/program.json") :: IO (Maybe Program)) >>=
       \decoded -> case decoded of
         Just program -> putStrLn (show program) >> putStrLn (show $ Program.check program) >>
           putStrLn (show $ Machine.execute (Tape.fromString "111-11=") program)
         Nothing -> putStrLn "parsing error"
