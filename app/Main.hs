module Main where

import qualified BusySignal
import Data.List
import Data.String
import PdDSL

main :: IO ()
main = putStrLn $ compile graph
  where
    graph = BusySignal.graph
