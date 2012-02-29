{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Nlopt.Wrappers( -- * misc stuff
                       nloptAlgorithmName
                     , nloptSRand
                     , nloptSRandTime
                     , nloptVersion
                     -- * api
                     , nloptCreate
                     , nloptCopy
                     , nloptOptimize
                     , nloptSetMinObjective
                     , nloptSetMaxObjective
                     , nloptGetAlgorithm
                     , nloptGetDimension
                     -- * constraints
                     , nloptSetLowerBounds
                     , nloptSetLowerBounds1
                     , nloptGetLowerBounds
                     , nloptSetUpperBounds
                     , nloptSetUpperBounds1
                     , nloptGetUpperBounds
                     , nloptRemoveInequalityConstraints
                     , nloptAddInequalityConstraint
                     , nloptAddInequalityMConstraint
                     , nloptRemoveEqualityConstraints
                     , nloptAddEqualityConstraint
                     , nloptAddEqualityMConstraint
                     -- * stopping criteria
                     , nloptSetStopVal
                     , nloptGetStopval
                     , nloptSetFtolRel
                     , nloptGetFtolRel
                     , nloptSetFtolAbs
                     , nloptGetFtolAbs
                     , nloptSetXtolRel
                     , nloptGetXtolRel
                     , nloptSetXtolAbs1
                     , nloptSetXtolAbs
                     , nloptGetXtolAbs
                     , nloptSetMaxeval
                     , nloptGetMaxeval
                     , nloptSetMaxtime
                     , nloptGetMaxtime
                     , nloptForceStop
                     , nloptSetForceStop
                     , nloptGetForceStop
                     -- * more algorithm-specific parameters
                     , nloptSetLocalOptimizer
                     , nloptSetPopulation
                     , nloptGetPopulation
                     , nloptSetVectorStorage
                     , nloptGetVectorStorage
                     , nloptSetDefaultInitialStep
                     , nloptSetInitialStep
                     , nloptSetInitialStep1
                     , nloptGetInitialStep
                     ) where

import Foreign.C
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(liftM)

import Nlopt.Enums
import Nlopt.Bindings

data NloptOpt = NloptOpt (ForeignPtr S_nlopt_opt_s) deriving Show


----------------------------------------------------------------------------------
----------------------------------- misc -----------------------------------------
----------------------------------------------------------------------------------

--c_nlopt_algorithm_name :: T_nlopt_algorithm -> IO (Ptr CChar)
nloptAlgorithmName :: NloptAlgorithm -> IO String
nloptAlgorithmName alg = do
  char <- c_nlopt_algorithm_name (algorithmToCInt alg)
  string <- peekCString char
  return string

--c_nlopt_srand :: CULong -> IO ()
nloptSRand :: Int -> IO ()
nloptSRand = c_nlopt_srand . fromIntegral

--c_nlopt_srand_time :: IO ()
nloptSRandTime :: IO ()
nloptSRandTime = c_nlopt_srand_time

--c_nlopt_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
nloptVersion :: (Int,Int,Int)
nloptVersion = unsafePerformIO $ do
  d1' <- malloc
  d2' <- malloc
  d3' <- malloc
  c_nlopt_version d1' d2' d3'
  d1 <- peek d1'
  d2 <- peek d2'
  d3 <- peek d3'
  free d1'
  free d2'
  free d3'
  return (fromIntegral d1, fromIntegral d2, fromIntegral d3)


----------------------------------------------------------------------------------
------------------------------------- API ----------------------------------------
----------------------------------------------------------------------------------

--c_nlopt_create :: T_nlopt_algorithm -> CUInt -> IO (Ptr S_nlopt_opt_s)
nloptCreate :: NloptAlgorithm -> Int -> IO NloptOpt
nloptCreate algorithm dimensions = do
  unfinalized <- (c_nlopt_create (algorithmToCInt algorithm) (fromIntegral dimensions))
  optRaw <- newForeignPtr c_nlopt_destroy unfinalized
  return (NloptOpt optRaw)

--c_nlopt_destroy :: FunPtr (Ptr S_nlopt_opt_s -> IO ())
-- (don't need a wrapper)

--c_nlopt_copy :: Ptr S_nlopt_opt_s -> IO (Ptr S_nlopt_opt_s)
nloptCopy :: NloptOpt -> IO NloptOpt
nloptCopy (NloptOpt optRaw) = do
  unfinalized <- withForeignPtr optRaw c_nlopt_copy
  optRaw2 <- newForeignPtr c_nlopt_destroy unfinalized
  return (NloptOpt optRaw2)

--c_nlopt_optimize :: Ptr S_nlopt_opt_s -> Ptr CDouble -> Ptr CDouble -> IO T_nlopt_result
nloptOptimize :: NloptOpt -> IO ([Double], Double, NloptResult)
nloptOptimize opt@(NloptOpt optRaw) = do
  dim <- nloptGetDimension opt
  optX' <- mallocArray dim
  optF' <- malloc
  result <- liftM nloptResultFromCInt $ withForeignPtr optRaw (\x -> c_nlopt_optimize x optX' optF')
  optX <- peekArray dim optX'
  optF <- peek optF'
  free optX'
  free optF'
  return (map realToFrac optX, realToFrac optF, result)

--c_nlopt_set_min_objective :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> IO T_nlopt_result
nloptSetMinObjective :: NloptOpt -> T_nlopt_func -> IO NloptResult
nloptSetMinObjective = setObjective w_nlopt_set_min_objective_1 c_nlopt_set_min_objective

--c_nlopt_set_max_objective :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> IO T_nlopt_result
nloptSetMaxObjective :: NloptOpt -> T_nlopt_func -> IO NloptResult
nloptSetMaxObjective = setObjective w_nlopt_set_max_objective_1 c_nlopt_set_max_objective

--c_nlopt_get_algorithm :: Ptr S_nlopt_opt_s -> IO T_nlopt_algorithm
nloptGetAlgorithm :: NloptOpt -> IO NloptAlgorithm
nloptGetAlgorithm (NloptOpt optRaw) = liftM algorithmFromCInt $ withForeignPtr optRaw c_nlopt_get_algorithm

--c_nlopt_get_dimension :: Ptr S_nlopt_opt_s -> IO CUInt
nloptGetDimension :: NloptOpt -> IO Int
nloptGetDimension (NloptOpt optRaw) = liftM fromIntegral $ withForeignPtr optRaw c_nlopt_get_dimension


----------------------------------------------------------------------------------
------------------------------- constraints --------------------------------------
----------------------------------------------------------------------------------

--c_nlopt_set_lower_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptSetLowerBounds :: NloptOpt -> [Double] -> IO NloptResult
nloptSetLowerBounds = setDoubles c_nlopt_set_lower_bounds
  
--c_nlopt_set_lower_bounds1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetLowerBounds1 :: NloptOpt -> Double -> IO NloptResult
nloptSetLowerBounds1 = setDouble c_nlopt_set_lower_bounds1
  
--c_nlopt_get_lower_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptGetLowerBounds :: NloptOpt -> IO ([Double], NloptResult)
nloptGetLowerBounds opt@(NloptOpt optRaw) = do
  dim <- nloptGetDimension opt
  xLb' <- mallocArray dim
  result <- withForeignPtr optRaw $ flip c_nlopt_get_lower_bounds xLb'
  xLb <- peekArray dim xLb'
  free xLb'
  return (map realToFrac xLb, nloptResultFromCInt result)
  
--c_nlopt_set_upper_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptSetUpperBounds :: NloptOpt -> [Double] -> IO NloptResult
nloptSetUpperBounds = setDoubles c_nlopt_set_upper_bounds
  
--c_nlopt_set_upper_bounds1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetUpperBounds1 :: NloptOpt -> Double -> IO NloptResult
nloptSetUpperBounds1 = setDouble c_nlopt_set_upper_bounds1

--c_nlopt_get_upper_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptGetUpperBounds :: NloptOpt -> IO ([Double], NloptResult)
nloptGetUpperBounds opt@(NloptOpt optRaw) = do
  dim <- nloptGetDimension opt
  xUb' <- mallocArray dim
  result <- withForeignPtr optRaw $ flip c_nlopt_get_upper_bounds xUb'
  xUb <- peekArray dim xUb'
  free xUb'
  return (map realToFrac xUb, nloptResultFromCInt result)
  
--c_nlopt_remove_inequality_constraints :: Ptr S_nlopt_opt_s -> IO T_nlopt_result
nloptRemoveInequalityConstraints :: NloptOpt -> IO NloptResult
nloptRemoveInequalityConstraints = removeConstraints c_nlopt_remove_inequality_constraints
  
--c_nlopt_add_inequality_constraint :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> CDouble -> IO T_nlopt_result
nloptAddInequalityConstraint :: NloptOpt -> T_nlopt_func -> Double -> IO NloptResult
nloptAddInequalityConstraint = addConstraint c_nlopt_add_inequality_constraint w_nlopt_add_inequality_constraint_1

--c_nlopt_add_inequality_mconstraint :: Ptr S_nlopt_opt_s -> CUInt -> FunPtr T_nlopt_mfunc -> Ptr CChar -> Ptr CDouble -> IO T_nlopt_result
nloptAddInequalityMConstraint :: NloptOpt -> T_nlopt_mfunc -> [Double] -> IO NloptResult
nloptAddInequalityMConstraint =
  addMConstraint c_nlopt_add_inequality_mconstraint w_nlopt_add_inequality_mconstraint_1

--c_nlopt_remove_equality_constraints :: Ptr S_nlopt_opt_s -> IO T_nlopt_result
nloptRemoveEqualityConstraints :: NloptOpt -> IO NloptResult
nloptRemoveEqualityConstraints (NloptOpt optRaw) =
  liftM nloptResultFromCInt $ withForeignPtr optRaw c_nlopt_remove_equality_constraints
  
--c_nlopt_add_equality_constraint :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> CDouble -> IO T_nlopt_result
nloptAddEqualityConstraint :: NloptOpt -> T_nlopt_func -> Double -> IO NloptResult
nloptAddEqualityConstraint = addConstraint c_nlopt_add_equality_constraint w_nlopt_add_equality_constraint_1
  
--c_nlopt_add_equality_mconstraint :: Ptr S_nlopt_opt_s -> CUInt -> FunPtr T_nlopt_mfunc -> Ptr CChar -> Ptr CDouble -> IO T_nlopt_result
nloptAddEqualityMConstraint :: NloptOpt -> T_nlopt_mfunc -> [Double] -> IO NloptResult
nloptAddEqualityMConstraint =
  addMConstraint c_nlopt_add_equality_mconstraint w_nlopt_add_equality_mconstraint_1


----------------------------------------------------------------------------------
----------------------------- stopping criteria ----------------------------------
----------------------------------------------------------------------------------

--c_nlopt_set_stopval :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetStopVal :: NloptOpt -> Double -> IO NloptResult
nloptSetStopVal = setDouble c_nlopt_set_stopval

--c_nlopt_get_stopval :: Ptr S_nlopt_opt_s -> IO CDouble
nloptGetStopval :: NloptOpt -> IO Double
nloptGetStopval = getDouble c_nlopt_get_stopval

--c_nlopt_set_ftol_rel :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetFtolRel :: NloptOpt -> Double -> IO NloptResult
nloptSetFtolRel = setDouble c_nlopt_set_ftol_rel

--c_nlopt_get_ftol_rel :: Ptr S_nlopt_opt_s -> IO CDouble
nloptGetFtolRel :: NloptOpt -> IO Double
nloptGetFtolRel = getDouble c_nlopt_get_ftol_rel

--c_nlopt_set_ftol_abs :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetFtolAbs :: NloptOpt -> Double -> IO NloptResult
nloptSetFtolAbs = setDouble c_nlopt_set_ftol_abs

--c_nlopt_get_ftol_abs :: Ptr S_nlopt_opt_s -> IO CDouble
nloptGetFtolAbs :: NloptOpt -> IO Double
nloptGetFtolAbs = getDouble c_nlopt_get_ftol_abs

--c_nlopt_set_xtol_rel :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetXtolRel :: NloptOpt -> Double -> IO NloptResult
nloptSetXtolRel = setDouble c_nlopt_set_xtol_rel

--c_nlopt_get_xtol_rel :: Ptr S_nlopt_opt_s -> IO CDouble
nloptGetXtolRel :: NloptOpt -> IO Double
nloptGetXtolRel = getDouble c_nlopt_get_xtol_rel

--c_nlopt_set_xtol_abs1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetXtolAbs1 :: NloptOpt -> Double -> IO NloptResult
nloptSetXtolAbs1 = setDouble c_nlopt_set_xtol_abs1

--c_nlopt_set_xtol_abs :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptSetXtolAbs :: NloptOpt -> [Double] -> IO NloptResult
nloptSetXtolAbs = setDoubles c_nlopt_set_xtol_abs

--c_nlopt_get_xtol_abs :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptGetXtolAbs :: NloptOpt -> IO ([Double], NloptResult)
nloptGetXtolAbs opt@(NloptOpt optRaw) = do
  dim <- nloptGetDimension opt
  xtolAbs' <- mallocArray dim
  result <- withForeignPtr optRaw $ flip c_nlopt_get_xtol_abs xtolAbs'
  xtolAbs <- peekArray dim xtolAbs'
  free xtolAbs'
  return (map realToFrac xtolAbs, nloptResultFromCInt result)

--c_nlopt_set_maxeval :: Ptr S_nlopt_opt_s -> CInt -> IO T_nlopt_result
nloptSetMaxeval :: NloptOpt -> Int -> IO NloptResult
nloptSetMaxeval = setIntegral c_nlopt_set_maxeval

--c_nlopt_get_maxeval :: Ptr S_nlopt_opt_s -> IO CInt
nloptGetMaxeval :: NloptOpt -> IO Double
nloptGetMaxeval = getIntegral c_nlopt_get_maxeval

--c_nlopt_set_maxtime :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetMaxtime :: NloptOpt -> Double -> IO NloptResult
nloptSetMaxtime = setDouble c_nlopt_set_maxtime

--c_nlopt_get_maxtime :: Ptr S_nlopt_opt_s -> IO CDouble
nloptGetMaxtime :: NloptOpt -> IO Double
nloptGetMaxtime = getDouble c_nlopt_get_maxtime

--c_nlopt_force_stop :: Ptr S_nlopt_opt_s -> IO T_nlopt_result
nloptForceStop :: NloptOpt -> IO NloptResult
nloptForceStop (NloptOpt optRaw) = liftM nloptResultFromCInt $ withForeignPtr optRaw c_nlopt_force_stop

--c_nlopt_set_force_stop :: Ptr S_nlopt_opt_s -> CInt -> IO T_nlopt_result
nloptSetForceStop :: NloptOpt -> Int -> IO NloptResult
nloptSetForceStop = setIntegral c_nlopt_set_force_stop

--c_nlopt_get_force_stop :: Ptr S_nlopt_opt_s -> IO CInt
nloptGetForceStop :: NloptOpt -> IO Int
nloptGetForceStop = getIntegral c_nlopt_get_force_stop


----------------------------------------------------------------------------------
------------------- more algorithm-specific parameters ---------------------------
----------------------------------------------------------------------------------

--c_nlopt_set_local_optimizer :: Ptr S_nlopt_opt_s -> Ptr S_nlopt_opt_s -> IO T_nlopt_result
nloptSetLocalOptimizer :: NloptOpt -> NloptOpt -> IO NloptResult
nloptSetLocalOptimizer (NloptOpt optRaw) (NloptOpt optRawLocal) = do
  result <- withForeignPtr optRaw $ \optRaw' -> withForeignPtr optRawLocal $
    c_nlopt_set_local_optimizer optRaw'
  return (nloptResultFromCInt result)

--c_nlopt_set_population :: Ptr S_nlopt_opt_s -> CUInt -> IO T_nlopt_result
nloptSetPopulation :: NloptOpt -> Int -> IO NloptResult
nloptSetPopulation = setIntegral c_nlopt_set_population

--c_nlopt_get_population :: Ptr S_nlopt_opt_s -> IO CUInt
nloptGetPopulation :: NloptOpt -> IO Int
nloptGetPopulation = getIntegral c_nlopt_get_population

--c_nlopt_set_vector_storage :: Ptr S_nlopt_opt_s -> CUInt -> IO T_nlopt_result
nloptSetVectorStorage :: NloptOpt -> Int -> IO NloptResult
nloptSetVectorStorage = setIntegral c_nlopt_set_vector_storage

--c_nlopt_get_vector_storage :: Ptr S_nlopt_opt_s -> IO CUInt
nloptGetVectorStorage :: NloptOpt -> IO Int
nloptGetVectorStorage = getIntegral c_nlopt_get_vector_storage

--c_nlopt_set_default_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptSetDefaultInitialStep :: NloptOpt -> [Double] -> IO NloptResult
nloptSetDefaultInitialStep = setDoubles c_nlopt_set_default_initial_step

--c_nlopt_set_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result
nloptSetInitialStep :: NloptOpt -> [Double] -> IO NloptResult
nloptSetInitialStep = setDoubles c_nlopt_set_initial_step

--c_nlopt_set_initial_step1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result
nloptSetInitialStep1 :: NloptOpt -> Double -> IO NloptResult
nloptSetInitialStep1 = setDouble c_nlopt_set_initial_step1

--c_nlopt_get_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> Ptr CDouble -> IO T_nlopt_result
nloptGetInitialStep :: NloptOpt -> [Double] -> IO ([Double], NloptResult)
nloptGetInitialStep (NloptOpt optRaw) x = do
  let dim = length x
  x' <- newArray $ map realToFrac x
  dx' <- mallocArray dim
  result <- withForeignPtr optRaw $ \optRaw' -> c_nlopt_get_initial_step optRaw' x' dx'
  dx <- peekArray dim dx'
  free x'
  free dx'
  return (map realToFrac dx, nloptResultFromCInt result)


----------------------------------------------------------------------------------
------------------------- (reusable utility functions) ---------------------------
----------------------------------------------------------------------------------

setSomething :: (a -> b) -> (Ptr S_nlopt_opt_s -> b -> IO T_nlopt_result) -> NloptOpt -> a -> IO NloptResult
setSomething convert cFun (NloptOpt optRaw) x =
  liftM nloptResultFromCInt $ withForeignPtr optRaw $ flip cFun (convert x)
  
setDouble :: (Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result) -> NloptOpt -> Double -> IO NloptResult
setDouble = setSomething realToFrac

setIntegral :: (Integral a, Num b) => (Ptr S_nlopt_opt_s -> b -> IO T_nlopt_result) -> NloptOpt -> a -> IO NloptResult
setIntegral = setSomething fromIntegral

setDoubles :: (Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result) -> NloptOpt -> [Double] -> IO NloptResult
setDoubles cFun (NloptOpt optRaw) xs = do
  xs' <- newArray $ map realToFrac xs
  result <- withForeignPtr optRaw $ flip cFun xs'
  free xs'
  return (nloptResultFromCInt result)

getSomething :: (a -> b) -> (Ptr S_nlopt_opt_s -> IO a) -> NloptOpt -> IO b
getSomething convert cFun (NloptOpt optRaw) = liftM convert $ withForeignPtr optRaw cFun
  
getDouble :: (Ptr S_nlopt_opt_s -> IO CDouble) -> NloptOpt -> IO Double
getDouble = getSomething realToFrac

getIntegral :: (Integral a, Num b) => (Ptr S_nlopt_opt_s -> IO a) -> NloptOpt -> IO b
getIntegral = getSomething fromIntegral

removeConstraints :: (Ptr S_nlopt_opt_s -> IO T_nlopt_result) -> NloptOpt -> IO NloptResult
removeConstraints cFun (NloptOpt optRaw) = liftM nloptResultFromCInt $ withForeignPtr optRaw cFun

setObjective :: (T_nlopt_func -> IO (FunPtr T_nlopt_func))
                -> (Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> IO T_nlopt_result)
                -> NloptOpt
                -> T_nlopt_func
                -> IO NloptResult
setObjective wrapperFun cFun (NloptOpt optRaw) fun = do
  funPtr <- wrapperFun fun
  res <- withForeignPtr optRaw $ (\optRaw' -> cFun optRaw' funPtr nullPtr)
  return (nloptResultFromCInt res)

addConstraint :: (Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> CDouble -> IO T_nlopt_result)
                 -> (T_nlopt_func -> IO (FunPtr T_nlopt_func))
                 -> NloptOpt -> T_nlopt_func -> Double -> IO NloptResult
addConstraint cFun wrapperFun (NloptOpt optRaw) fun tol = do
  funPtr <- wrapperFun fun
  res <- withForeignPtr optRaw $ \optRaw' -> cFun optRaw' funPtr nullPtr (realToFrac tol)
  return (nloptResultFromCInt res)

addMConstraint :: (Ptr S_nlopt_opt_s -> CUInt -> FunPtr T_nlopt_mfunc -> Ptr CChar -> Ptr CDouble -> IO T_nlopt_result)
                 -> (T_nlopt_mfunc -> IO (FunPtr T_nlopt_mfunc))
                 -> NloptOpt -> T_nlopt_mfunc -> [Double] -> IO NloptResult
addMConstraint cFun wrapperFun (NloptOpt optRaw) fun tol = do
  let dim = length tol
  funPtr <- wrapperFun fun
  tol' <- newArray (map realToFrac tol)
  res <- withForeignPtr optRaw $ \optRaw' -> cFun optRaw' (fromIntegral dim) funPtr nullPtr tol'
  free tol'
  return (nloptResultFromCInt res)
