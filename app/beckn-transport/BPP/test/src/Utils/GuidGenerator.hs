{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.GuidGenerator where

import Beckn.Types.Common
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import EulerHS.Prelude

instance MonadGuid IO where
  generateGUIDText = UUID.toText <$> UUID.nextRandom
