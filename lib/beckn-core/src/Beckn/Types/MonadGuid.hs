module Beckn.Types.MonadGuid where

import EulerHS.Prelude

class Monad m => MonadGuid m where
  generateGUIDText :: m Text
