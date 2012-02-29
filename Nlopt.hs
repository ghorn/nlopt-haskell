{-# OPTIONS_GHC -Wall #-}

module Nlopt( optimize
            , defaultOptimize
            , defaultConfig
            , OptConfig(..)
            , module Nlopt.Enums
            ) where

import Nlopt.Wrappers
import Nlopt.Bindings
import Nlopt.Enums(NloptResult(..), NloptAlgorithm(..))

import Foreign
import Control.Monad(when)

mkFunNoDeriv :: ([Double] -> Double) -> T_nlopt_func
mkFunNoDeriv f' n xPtr gradPtr _ = do
  x <- peekArray (fromIntegral n) xPtr
  when (gradPtr /= nullPtr) (error "it wants a grad!!")
  return $ realToFrac $ f' (map realToFrac x)

--mkFunWithDeriv :: ([CDouble] -> [CDouble]) -> T_nlopt_func
--mkFunWithDeriv f' n xPtr gradPtr _ = do
----  putStrLn $ "n: " ++ show n
--  x <- peekArray (fromIntegral n) xPtr
--  putStrLn $ "current x: " ++ show x
--  when (gradPtr /= nullPtr) $ do
--    (error "it wants a grad!!")
--  let val = f' x
--  putStrLn $ "current f: " ++ show val
--  return val

data OptConfig = OptConfig { dim :: Int
                           , xBounds :: ([Double],[Double])
                           , xTolRel :: Double
                           , xTolAbs :: [Double]
                           , fTolRel :: Double
                           , fTolAbs :: Double
                           , algorithm :: NloptAlgorithm
                           , objFun :: [Double] -> Double
                           }

defaultOptimize :: ([Double] -> Double) -> NloptAlgorithm -> Int -> IO ([Double], Double, NloptResult)
defaultOptimize f alg n = optimize (defaultConfig f alg n)

defaultConfig :: ([Double] -> Double) -> NloptAlgorithm -> Int -> OptConfig
defaultConfig f alg n = OptConfig { dim = n
                                  , xBounds = (replicate n (-1e20), replicate n 1e22)
                                  , xTolRel = 0
                                  , xTolAbs = replicate n 1e-8
                                  , fTolRel = 0
                                  , fTolAbs = 0
                                  , algorithm = alg
                                  , objFun = f
                                  }

optimize :: OptConfig -> IO ([Double], Double, NloptResult)
optimize cfg = do
  opt <- nloptCreate (algorithm cfg) (dim cfg)
  _ <- nloptSetMinObjective opt $ mkFunNoDeriv (objFun cfg)

  _ <- nloptSetLowerBounds opt (fst (xBounds cfg))
  _ <- nloptSetUpperBounds opt (snd (xBounds cfg))

  _ <- nloptSetXtolRel opt (xTolRel cfg)
  _ <- nloptSetXtolAbs opt (xTolAbs cfg)
  
  _ <- nloptSetFtolRel opt (fTolRel cfg)
  _ <- nloptSetFtolAbs opt (fTolAbs cfg)

  ret@(xOpt, fOpt, res) <- nloptOptimize opt
  
  putStrLn ""
  print opt

  putStrLn $ "\nresult: " ++ show res
  putStrLn $ "xOpt:   " ++ show xOpt
  putStrLn $ "fOpt:   " ++ show fOpt
  return ret
