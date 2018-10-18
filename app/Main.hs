module Main where

import qualified Data.Map as Map
import Program 

main :: IO ()
main = putStrLn (show t)
       >> putStrLn (show $ Program.check t)
    where t = Program.new
