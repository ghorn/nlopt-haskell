{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Nlopt.Bindings( c_nlopt_algorithm_name
                     , c_nlopt_srand
                     , c_nlopt_srand_time
                     , c_nlopt_version
                       -- * api
                     , c_nlopt_create
                     , c_nlopt_destroy
                     , c_nlopt_copy
                     , c_nlopt_optimize
                     , c_nlopt_set_min_objective
                     , w_nlopt_set_min_objective_1
                     , c_nlopt_set_max_objective
                     , w_nlopt_set_max_objective_1
                     , c_nlopt_get_algorithm
                     , c_nlopt_get_dimension
                       -- * constraints
                     , c_nlopt_set_lower_bounds
                     , c_nlopt_set_lower_bounds1
                     , c_nlopt_get_lower_bounds
                     , c_nlopt_set_upper_bounds
                     , c_nlopt_set_upper_bounds1
                     , c_nlopt_get_upper_bounds
                     , c_nlopt_remove_inequality_constraints
                     , c_nlopt_add_inequality_constraint
                     , w_nlopt_add_inequality_constraint_1
                     , c_nlopt_add_inequality_mconstraint
                     , w_nlopt_add_inequality_mconstraint_1
                     , c_nlopt_remove_equality_constraints
                     , c_nlopt_add_equality_constraint
                     , w_nlopt_add_equality_constraint_1
                     , c_nlopt_add_equality_mconstraint
                     , w_nlopt_add_equality_mconstraint_1
                       -- * stopping criteria
                     , c_nlopt_set_stopval
                     , c_nlopt_get_stopval
                     , c_nlopt_set_ftol_rel
                     , c_nlopt_get_ftol_rel
                     , c_nlopt_set_ftol_abs
                     , c_nlopt_get_ftol_abs
                     , c_nlopt_set_xtol_rel
                     , c_nlopt_get_xtol_rel
                     , c_nlopt_set_xtol_abs1
                     , c_nlopt_set_xtol_abs
                     , c_nlopt_get_xtol_abs
                     , c_nlopt_set_maxeval
                     , c_nlopt_get_maxeval
                     , c_nlopt_set_maxtime
                     , c_nlopt_get_maxtime
                     , c_nlopt_force_stop
                     , c_nlopt_set_force_stop
                     , c_nlopt_get_force_stop
                       -- * more algorithm-specific parameters
                     , c_nlopt_set_local_optimizer
                     , c_nlopt_set_population
                     , c_nlopt_get_population
                     , c_nlopt_set_vector_storage
                     , c_nlopt_get_vector_storage
                     , c_nlopt_set_default_initial_step
                     , c_nlopt_set_initial_step
                     , c_nlopt_set_initial_step1
                     , c_nlopt_get_initial_step
                     , T_nlopt_func
                     , T_nlopt_mfunc
                     , S_nlopt_opt_s(..)
                     ) where

import Foreign(Ptr, FunPtr)
import Foreign.C.Types(CUInt(..), CInt(..), CDouble(..), CChar, CULong(..))

import Nlopt.Enums(T_nlopt_result, T_nlopt_algorithm)

type T_nlopt_func = CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> IO CDouble
type T_nlopt_mfunc = CUInt -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CChar -> IO ()
data S_nlopt_opt_s = S_nlopt_opt_s


foreign import ccall "static /usr/local/include/nlopt.h nlopt_algorithm_name"
  c_nlopt_algorithm_name :: T_nlopt_algorithm -> IO (Ptr CChar)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_srand"
  c_nlopt_srand :: CULong -> IO ()

foreign import ccall "static /usr/local/include/nlopt.h nlopt_srand_time"
  c_nlopt_srand_time :: IO ()

foreign import ccall "static /usr/local/include/nlopt.h nlopt_version"
  c_nlopt_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

----------------------------------------------------------------------------------

foreign import ccall "static /usr/local/include/nlopt.h nlopt_create"
  c_nlopt_create :: T_nlopt_algorithm -> CUInt -> IO (Ptr S_nlopt_opt_s)

foreign import ccall "static /usr/local/include/nlopt.h &nlopt_destroy"
  c_nlopt_destroy :: FunPtr (Ptr S_nlopt_opt_s -> IO ())

foreign import ccall "static /usr/local/include/nlopt.h nlopt_copy"
  c_nlopt_copy :: Ptr S_nlopt_opt_s -> IO (Ptr S_nlopt_opt_s)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_optimize"
  c_nlopt_optimize :: Ptr S_nlopt_opt_s -> Ptr CDouble -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_min_objective"
  c_nlopt_set_min_objective :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_set_min_objective_1 :: T_nlopt_func -> IO (FunPtr T_nlopt_func)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_max_objective"
  c_nlopt_set_max_objective :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_set_max_objective_1 :: T_nlopt_func -> IO (FunPtr T_nlopt_func)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_algorithm"
  c_nlopt_get_algorithm :: Ptr S_nlopt_opt_s -> IO T_nlopt_algorithm

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_dimension"
  c_nlopt_get_dimension :: Ptr S_nlopt_opt_s -> IO CUInt


-- /* constraints: */

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_lower_bounds"
  c_nlopt_set_lower_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_lower_bounds1"
  c_nlopt_set_lower_bounds1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_lower_bounds"
  c_nlopt_get_lower_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_upper_bounds"
  c_nlopt_set_upper_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_upper_bounds1"
  c_nlopt_set_upper_bounds1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_upper_bounds"
  c_nlopt_get_upper_bounds :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_remove_inequality_constraints"
  c_nlopt_remove_inequality_constraints :: Ptr S_nlopt_opt_s -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_add_inequality_constraint"
  c_nlopt_add_inequality_constraint :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> CDouble -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_add_inequality_constraint_1 :: T_nlopt_func -> IO (FunPtr T_nlopt_func)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_add_inequality_mconstraint"
  c_nlopt_add_inequality_mconstraint :: Ptr S_nlopt_opt_s -> CUInt -> FunPtr T_nlopt_mfunc -> Ptr CChar -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_add_inequality_mconstraint_1 :: T_nlopt_mfunc -> IO (FunPtr T_nlopt_mfunc)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_remove_equality_constraints"
  c_nlopt_remove_equality_constraints :: Ptr S_nlopt_opt_s -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_add_equality_constraint"
  c_nlopt_add_equality_constraint :: Ptr S_nlopt_opt_s -> FunPtr T_nlopt_func -> Ptr CChar -> CDouble -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_add_equality_constraint_1 :: T_nlopt_func -> IO (FunPtr T_nlopt_func)

foreign import ccall "static /usr/local/include/nlopt.h nlopt_add_equality_mconstraint"
  c_nlopt_add_equality_mconstraint :: Ptr S_nlopt_opt_s -> CUInt -> FunPtr T_nlopt_mfunc -> Ptr CChar -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "wrapper"
  w_nlopt_add_equality_mconstraint_1 :: T_nlopt_mfunc -> IO (FunPtr T_nlopt_mfunc)


-- /* stopping criteria: */

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_stopval"
  c_nlopt_set_stopval :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_stopval"
  c_nlopt_get_stopval :: Ptr S_nlopt_opt_s -> IO CDouble

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_ftol_rel"
  c_nlopt_set_ftol_rel :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_ftol_rel"
  c_nlopt_get_ftol_rel :: Ptr S_nlopt_opt_s -> IO CDouble

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_ftol_abs"
  c_nlopt_set_ftol_abs :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_ftol_abs"
  c_nlopt_get_ftol_abs :: Ptr S_nlopt_opt_s -> IO CDouble

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_xtol_rel"
  c_nlopt_set_xtol_rel :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_xtol_rel"
  c_nlopt_get_xtol_rel :: Ptr S_nlopt_opt_s -> IO CDouble

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_xtol_abs1"
  c_nlopt_set_xtol_abs1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_xtol_abs"
  c_nlopt_set_xtol_abs :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_xtol_abs"
  c_nlopt_get_xtol_abs :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_maxeval"
  c_nlopt_set_maxeval :: Ptr S_nlopt_opt_s -> CInt -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_maxeval"
  c_nlopt_get_maxeval :: Ptr S_nlopt_opt_s -> IO CInt

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_maxtime"
  c_nlopt_set_maxtime :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_maxtime"
  c_nlopt_get_maxtime :: Ptr S_nlopt_opt_s -> IO CDouble

foreign import ccall "static /usr/local/include/nlopt.h nlopt_force_stop"
  c_nlopt_force_stop :: Ptr S_nlopt_opt_s -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_force_stop"
  c_nlopt_set_force_stop :: Ptr S_nlopt_opt_s -> CInt -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_force_stop"
  c_nlopt_get_force_stop :: Ptr S_nlopt_opt_s -> IO CInt


-- /* more algorithm-specific parameters */

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_local_optimizer"
  c_nlopt_set_local_optimizer :: Ptr S_nlopt_opt_s -> Ptr S_nlopt_opt_s -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_population"
  c_nlopt_set_population :: Ptr S_nlopt_opt_s -> CUInt -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_population"
  c_nlopt_get_population :: Ptr S_nlopt_opt_s -> IO CUInt

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_vector_storage"
  c_nlopt_set_vector_storage :: Ptr S_nlopt_opt_s -> CUInt -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_vector_storage"
  c_nlopt_get_vector_storage :: Ptr S_nlopt_opt_s -> IO CUInt

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_default_initial_step"
  c_nlopt_set_default_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_initial_step"
  c_nlopt_set_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_set_initial_step1"
  c_nlopt_set_initial_step1 :: Ptr S_nlopt_opt_s -> CDouble -> IO T_nlopt_result

foreign import ccall "static /usr/local/include/nlopt.h nlopt_get_initial_step"
  c_nlopt_get_initial_step :: Ptr S_nlopt_opt_s -> Ptr CDouble -> Ptr CDouble -> IO T_nlopt_result
