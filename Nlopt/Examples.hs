{-# OPTIONS_GHC -Wall #-}

module Nlopt.Examples( anExample
                     , anotherExample
                     ) where

import Nlopt

-- the function to be minimized
f :: Num a => [a] -> a
f [x,y] = 10*(x-1)*(x-1) + 20*(y-2)*(y-2) + 30
f _ = error "fuck"

---- exact gradient
--df :: Num a => [a] -> [a]
--df [x,y] = [20*(x-1), 40*(y-2)]
--df _ = error "fuck"

g :: Num a => [a] -> a
g [x] = (x-1)*(x-1)
g _ = error "FUCK"

anExample :: IO ()
anExample = do
  _ <- defaultOptimize f NLOPT_LN_COBYLA 2
  return ()

anotherExample :: IO ()
anotherExample = do
  _ <- defaultOptimize g NLOPT_LN_NELDERMEAD 1
  return ()
