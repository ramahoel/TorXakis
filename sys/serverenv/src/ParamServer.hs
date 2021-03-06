{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module ParamServer

-- ----------------------------------------------------------------------------------------- --
-- 
-- TorXakis Server Parameters
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( Params           -- Params = Map.Map String (String,String->Bool)
, initParams       -- initPrarams :: Map.Map String (String,String->Bool)
                   -- initial values of parameters
)

where

import qualified Data.Char as Char
import qualified Data.Map  as Map

-- ----------------------------------------------------------------------------------------- --
-- initParams

type  Params  =  Map.Map String (String,String->Bool)

initParams :: Params
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- sut observations
  
  [ ( "param_Sut_deltaTime"      , "2000"      , all Char.isDigit)
            -- param_Sut_deltaTime :: Int (>0)
            -- quiescence output time (millisec >0)

  , ( "param_Sut_ioTime"         , "10"        , all Char.isDigit)
            -- param_Sut_ioTime :: Int (>0)
            --  timeout for input when trying output (msec, >0)

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_Sim_deltaTime"      , "200"       , all Char.isDigit)
            -- param_Sim_deltaTime :: Int (>0)
            -- quiescence input time (millisec >0)
  ]

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
