module Beckn.Types.MonadGuid where

import Beckn.Types.Flow
import EulerHS.Language as L
import EulerHS.Prelude

class Monad m => MonadGuid m where
  generateGUIDText :: m Text

instance MonadGuid (FlowR r) where
  generateGUIDText = L.generateGUID
