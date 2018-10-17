module Main where

import qualified Data.Map as Map
import Machine 

main :: IO ()
main = putStrLn (show t)
       >> putStrLn (show $ Machine.check t)
       >> putStrLn (show $ Map.singleton 0 'a')
    where t = Machine.new
