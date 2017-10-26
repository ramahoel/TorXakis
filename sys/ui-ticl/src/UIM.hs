-- | User interface monad.
module UIM (UIM, UIEnv(..), empty) where

import           Control.Monad.Trans.State.Strict
import           TxsDefs                          (TxsDefs)
import qualified TxsDefs

type UIM = StateT UIEnv IO

-- | UI environment.
newtype UIEnv = UIEnv
    { tdefs :: TxsDefs -- ^ TorXakis definitions.
    }

-- TODO: maybe define a monoid instance.
empty :: UIEnv
empty = UIEnv { tdefs = TxsDefs.empty }
