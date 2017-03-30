{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- {-# OPTIONS_GHC -Wno-unused-binds   #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- To keep ghci happy, it appears that the plugin flag must be in the test module.
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}

-- {-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}
-- {-# OPTIONS_GHC -dverbose-core2core #-}

-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

-- Does this flag make any difference?
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

-- Tweak simpl-tick-factor from default of 100
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
-- {-# OPTIONS_GHC -fsimpl-tick-factor=250 #-}
-- {-# OPTIONS_GHC -fsimpl-tick-factor=5  #-}

{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

module Main where

import Control.Arrow (second,runKleisli)
import Data.Tuple (swap)
import Data.Maybe
-- import Distribution.TestSuite
import GHC.Generics hiding (R,D)
import GHC.Exts (lazy,coerce)

import ConCat.Misc (Unop,Binop,(:*),PseudoFun(..),R,bottom,oops,Yes2,sqr,magSqr)
import ConCat.Rep
import ConCat.Standardize
import ConCat.Free.VectorSpace (V)
import ConCat.Free.LinearRow
import ConCat.Incremental
import ConCat.AD
import ConCat.Interval
import ConCat.Syntactic (Syn,render)
import ConCat.Circuit (GenBuses)
import qualified ConCat.RunCircuit as RC
import ConCat.GLSL (genGlsl,CAnim)
import ConCat.Image
import ConCat.RunCircuit (go,Okay,(:>))
import ConCat.AltCat ( ccc,reveal,Uncurriable(..),U2(..),(:**:)(..),Ok2
                     , reprC,abstC,mulC,amb,ambC,asKleisli, recipC )
import qualified ConCat.AltCat as A
import ConCat.Rebox () -- experiment
import ConCat.Orphans ()
import ConCat.GradientDescent

import Gather

default (Int, Double)

main :: IO ()
main = print (gather (ccc (\ (x,y) -> x + y - y :: Int)) (10,20))
