module Main where

import           CmdsHandler
import           CmdsParser
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           System.Console.Haskeline         hiding (handle)
import           UIM

main :: IO ()
main = evalStateT (runInputT defaultSettings loop) empty
   where
-- The UI performs at the beginning "START" followed by "INIT"
-- Files are parsed at the "INIT" command
-- "START" doesn't seem to do much...
       loop :: InputT UIM ()
       loop = do
           line <- getInputLine "TXS >> "
           case line of
               Nothing     -> loop
               Just "quit" -> return ()
               Just input  -> do
                   lift $ handleInput input
                   loop


-- | Parse the input given by the user and dispatch the right command upon
-- successful parser.
handleInput :: String -> UIM ()
handleInput input =
    case parse input of
        Left err  -> reportErr err
        Right cmd -> handle cmd
