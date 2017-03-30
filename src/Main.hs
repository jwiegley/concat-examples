{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import ConCat.AltCat (ccc)
import ConCat.Syntactic (Syn,render)

import Gather

main :: IO ()
main = print (gather (ccc (\ (x,y) -> x - 3 + 7 * y :: Int)) (10,20))
