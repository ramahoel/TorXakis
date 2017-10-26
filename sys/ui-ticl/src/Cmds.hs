-- | Commands supported by the Ticl UI.

module Cmds (Cmd(..)) where

data Cmd
    = Load [FilePath] -- ^ Parse a list of TorXakis model files.
    | Stepper String  -- ^ Start the stepper using the given model.
    | StepN Int       -- ^ Perform N steps in the model.
    | StepA String    -- ^ Perform a step using the given action.
    deriving (Show, Eq)
