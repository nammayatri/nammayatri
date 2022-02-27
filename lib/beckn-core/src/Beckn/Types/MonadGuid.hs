module Beckn.Types.MonadGuid where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import EulerHS.Prelude

class Monad m => MonadGuid m where
  generateGUIDText :: m Text

generateGUIDTextIO :: IO Text
generateGUIDTextIO = UUID.toText <$> liftIO UUID.nextRandom
