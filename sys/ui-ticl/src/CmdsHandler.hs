module CmdsHandler (handle, reportErr) where

import           Cmds
import           Control.Monad.Trans.Class
import           Types
import           UIM

handle :: Cmd -> UIM ()
handle cmd = lift $ putStrLn $ "TODO: handle " ++ show cmd

reportErr :: Error -> UIM ()
reportErr = lift . putStrLn . ("Error: " ++)
