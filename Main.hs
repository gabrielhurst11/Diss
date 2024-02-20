module Main where

import Examples
import Propositional
import Cnf
import Functions

main :: IO ()
main = do
    putStrLn "Welcome to the Propositional Logic Solver!"
    putStrLn "Here are some example propositions:"
    print p1
    print p2
