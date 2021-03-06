{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Benchmarks.Enable (benchmarksSet) where

import           Benchmarks.Common
import           Paths
import           Prelude           hiding (FilePath)
import           Sqatt

benchDir :: FilePath
benchDir = "Enable"

modelFiles :: [FilePath]
modelFiles = [ txsFilePath BenchTest benchDir "Enable"
             , txsFilePath BenchTest "Sequence" "SingleActionSequence"
             ]

seqEnable :: TxsExample
seqEnable = TxsExample
    { exampleName = "sequence of enable operators, without data"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "SeqEnable"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

seqEnableInt :: TxsExample
seqEnableInt = TxsExample
    { exampleName = "sequence of enable operators, with integers"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "SeqEnableInt"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

seqEnableTwoInts :: TxsExample
seqEnableTwoInts = TxsExample
    { exampleName = "sequence of enable operators, with integers and two outputs"
    , txsModelFiles = modelFiles
    , txsCmdsFiles = [ seedSetupCmdFile
                     , txsCmdPath BenchTest benchDir "SeqEnableTwoInts"
                     ]
    , txsServerArgs = []
    , sutExample = Nothing
    , expectedResult = Pass
    }

benchmarksSet :: TxsExampleSet
benchmarksSet = TxsExampleSet "Enable" [ seqEnable
                                       , seqEnableInt
                                       , seqEnableTwoInts
                                       ]
