{-# OPTIONS_GHC -Wall #-}

module Nlopt.Enums( NloptAlgorithm(..)
                  , NloptResult(..)
                  , algorithmToCInt
                  , algorithmFromCInt
                  , nloptResultFromCInt
                  , T_nlopt_algorithm
                  , T_nlopt_result
                  ) where

import Foreign.C.Types

{- Naming conventions:
  
  NLOPT_{G/L}{D/N}_* = global/local derivative/no-derivative optimization, respectively 
 
  *_RAND algorithms involve some randomization.
 
  *_NOSCAL algorithms are *not* scaled to a unit hypercube (i.e. they are sensitive to the units of x)

-}
type T_nlopt_algorithm = CInt
type T_nlopt_result = CInt

data NloptAlgorithm = NLOPT_GN_DIRECT
                    | NLOPT_GN_DIRECT_L
                    | NLOPT_GN_DIRECT_L_RAND
                    | NLOPT_GN_DIRECT_NOSCAL
                    | NLOPT_GN_DIRECT_L_NOSCAL
                    | NLOPT_GN_DIRECT_L_RAND_NOSCAL
                    | NLOPT_GN_ORIG_DIRECT
                    | NLOPT_GN_ORIG_DIRECT_L
                    | NLOPT_GD_STOGO
                    | NLOPT_GD_STOGO_RAND
                    | NLOPT_LD_LBFGS_NOCEDAL
                    | NLOPT_LD_LBFGS
                    | NLOPT_LN_PRAXIS
                    | NLOPT_LD_VAR1
                    | NLOPT_LD_VAR2
                    | NLOPT_LD_TNEWTON
                    | NLOPT_LD_TNEWTON_RESTART
                    | NLOPT_LD_TNEWTON_PRECOND
                    | NLOPT_LD_TNEWTON_PRECOND_RESTART
                    | NLOPT_GN_CRS2_LM
                    | NLOPT_GN_MLSL
                    | NLOPT_GD_MLSL
                    | NLOPT_GN_MLSL_LDS
                    | NLOPT_GD_MLSL_LDS
                    | NLOPT_LD_MMA
                    | NLOPT_LN_COBYLA
                    | NLOPT_LN_NEWUOA
                    | NLOPT_LN_NEWUOA_BOUND
                    | NLOPT_LN_NELDERMEAD
                    | NLOPT_LN_SBPLX
                    | NLOPT_LN_AUGLAG
                    | NLOPT_LD_AUGLAG
                    | NLOPT_LN_AUGLAG_EQ
                    | NLOPT_LD_AUGLAG_EQ
                    | NLOPT_LN_BOBYQA
                    | NLOPT_GN_ISRES
                    | NLOPT_AUGLAG
                    | NLOPT_AUGLAG_EQ
                    | NLOPT_G_MLSL
                    | NLOPT_G_MLSL_LDS
                    | NLOPT_LD_SLSQP
                    | NLOPT_NUM_ALGORITHMS deriving (Eq, Show)

data NloptResult = NLOPT_FAILURE
                 | NLOPT_INVALID_ARGS
                 | NLOPT_OUT_OF_MEMORY
                 | NLOPT_ROUNDOFF_LIMITED
                 | NLOPT_FORCED_STOP
                 | NLOPT_SUCCESS
                 | NLOPT_STOPVAL_REACHED
                 | NLOPT_FTOL_REACHED
                 | NLOPT_XTOL_REACHED
                 | NLOPT_MAXEVAL_REACHED
                 | NLOPT_MAXTIME_REACHED deriving (Eq, Show)

algorithmToCInt :: NloptAlgorithm -> T_nlopt_algorithm
algorithmToCInt NLOPT_GN_DIRECT = 0
algorithmToCInt NLOPT_GN_DIRECT_L = 1
algorithmToCInt NLOPT_GN_DIRECT_L_RAND = 2
algorithmToCInt NLOPT_GN_DIRECT_NOSCAL = 3
algorithmToCInt NLOPT_GN_DIRECT_L_NOSCAL = 4
algorithmToCInt NLOPT_GN_DIRECT_L_RAND_NOSCAL = 5
algorithmToCInt NLOPT_GN_ORIG_DIRECT = 6
algorithmToCInt NLOPT_GN_ORIG_DIRECT_L = 7
algorithmToCInt NLOPT_GD_STOGO = 8
algorithmToCInt NLOPT_GD_STOGO_RAND = 9
algorithmToCInt NLOPT_LD_LBFGS_NOCEDAL = 10
algorithmToCInt NLOPT_LD_LBFGS = 11
algorithmToCInt NLOPT_LN_PRAXIS = 12
algorithmToCInt NLOPT_LD_VAR1 = 13
algorithmToCInt NLOPT_LD_VAR2 = 14
algorithmToCInt NLOPT_LD_TNEWTON = 15
algorithmToCInt NLOPT_LD_TNEWTON_RESTART = 16
algorithmToCInt NLOPT_LD_TNEWTON_PRECOND = 17
algorithmToCInt NLOPT_LD_TNEWTON_PRECOND_RESTART = 18
algorithmToCInt NLOPT_GN_CRS2_LM = 19
algorithmToCInt NLOPT_GN_MLSL = 20
algorithmToCInt NLOPT_GD_MLSL = 21
algorithmToCInt NLOPT_GN_MLSL_LDS = 22
algorithmToCInt NLOPT_GD_MLSL_LDS = 23
algorithmToCInt NLOPT_LD_MMA = 24
algorithmToCInt NLOPT_LN_COBYLA = 25
algorithmToCInt NLOPT_LN_NEWUOA = 26
algorithmToCInt NLOPT_LN_NEWUOA_BOUND = 27
algorithmToCInt NLOPT_LN_NELDERMEAD = 28
algorithmToCInt NLOPT_LN_SBPLX = 29
algorithmToCInt NLOPT_LN_AUGLAG = 30
algorithmToCInt NLOPT_LD_AUGLAG = 31
algorithmToCInt NLOPT_LN_AUGLAG_EQ = 32
algorithmToCInt NLOPT_LD_AUGLAG_EQ = 33
algorithmToCInt NLOPT_LN_BOBYQA = 34
algorithmToCInt NLOPT_GN_ISRES = 35
algorithmToCInt NLOPT_AUGLAG = 36
algorithmToCInt NLOPT_AUGLAG_EQ = 37
algorithmToCInt NLOPT_G_MLSL = 38
algorithmToCInt NLOPT_G_MLSL_LDS = 39
algorithmToCInt NLOPT_LD_SLSQP = 40
algorithmToCInt NLOPT_NUM_ALGORITHMS = 41

algorithmFromCInt :: T_nlopt_algorithm -> NloptAlgorithm
algorithmFromCInt k = case k of
  0 -> NLOPT_GN_DIRECT
  1 -> NLOPT_GN_DIRECT_L
  2 -> NLOPT_GN_DIRECT_L_RAND
  3 -> NLOPT_GN_DIRECT_NOSCAL
  4 -> NLOPT_GN_DIRECT_L_NOSCAL
  5 -> NLOPT_GN_DIRECT_L_RAND_NOSCAL
  6 -> NLOPT_GN_ORIG_DIRECT
  7 -> NLOPT_GN_ORIG_DIRECT_L
  8 -> NLOPT_GD_STOGO
  9 -> NLOPT_GD_STOGO_RAND
  10 -> NLOPT_LD_LBFGS_NOCEDAL
  11 -> NLOPT_LD_LBFGS
  12 -> NLOPT_LN_PRAXIS
  13 -> NLOPT_LD_VAR1
  14 -> NLOPT_LD_VAR2
  15 -> NLOPT_LD_TNEWTON
  16 -> NLOPT_LD_TNEWTON_RESTART
  17 -> NLOPT_LD_TNEWTON_PRECOND
  18 -> NLOPT_LD_TNEWTON_PRECOND_RESTART
  19 -> NLOPT_GN_CRS2_LM
  20 -> NLOPT_GN_MLSL
  21 -> NLOPT_GD_MLSL
  22 -> NLOPT_GN_MLSL_LDS
  23 -> NLOPT_GD_MLSL_LDS
  24 -> NLOPT_LD_MMA
  25 -> NLOPT_LN_COBYLA
  26 -> NLOPT_LN_NEWUOA
  27 -> NLOPT_LN_NEWUOA_BOUND
  28 -> NLOPT_LN_NELDERMEAD
  29 -> NLOPT_LN_SBPLX
  30 -> NLOPT_LN_AUGLAG
  31 -> NLOPT_LD_AUGLAG
  32 -> NLOPT_LN_AUGLAG_EQ
  33 -> NLOPT_LD_AUGLAG_EQ
  34 -> NLOPT_LN_BOBYQA
  35 -> NLOPT_GN_ISRES
  36 -> NLOPT_AUGLAG
  37 -> NLOPT_AUGLAG_EQ
  38 -> NLOPT_G_MLSL
  39 -> NLOPT_G_MLSL_LDS
  40 -> NLOPT_LD_SLSQP
  41 -> NLOPT_NUM_ALGORITHMS
  _  -> error $ "algorithmFromCInt got unrecognized value: "++show k

nloptResultFromCInt :: T_nlopt_result -> NloptResult
nloptResultFromCInt k = case k of
  -1 -> NLOPT_FAILURE
  -2 -> NLOPT_INVALID_ARGS
  -3 -> NLOPT_OUT_OF_MEMORY
  -4 -> NLOPT_ROUNDOFF_LIMITED
  -5 -> NLOPT_FORCED_STOP
  1  ->   NLOPT_SUCCESS
  2  ->   NLOPT_STOPVAL_REACHED
  3  ->   NLOPT_FTOL_REACHED
  4  ->   NLOPT_XTOL_REACHED
  5  ->   NLOPT_MAXEVAL_REACHED
  6  ->   NLOPT_MAXTIME_REACHED
  _  -> error $ "nloptResultFromCInt got unrecognized value: "++show k
