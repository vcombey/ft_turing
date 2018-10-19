module Main where

import qualified Data.Map as Map
import Program 
import Data.Aeson

main :: IO ()
main = ((decodeFileStrict "ressources/program.json") :: IO (Maybe Program)) >>=
       \decoded -> putStrLn (show decoded)
      {-- >> putStrLn (show $ Program.check t) --}
    --where t = Program.new
