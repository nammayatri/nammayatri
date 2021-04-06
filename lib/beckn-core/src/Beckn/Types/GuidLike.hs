module Beckn.Types.GuidLike where

import Beckn.Types.MonadGuid
import EulerHS.Prelude

class GuidLike m a where
  generateGUID :: m a

instance MonadGuid m => GuidLike m Text where
  generateGUID = generateGUIDText
