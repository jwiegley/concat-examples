{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

module Main where

import ConCat.AltCat (ccc)

import Gather

main :: IO ()
main = print (gather (ccc (\ (x,y) -> x + y - y :: Int)) (10,20))
