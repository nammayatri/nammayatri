{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Circle (Circle (..)) where

import Beckn.Types.Core.Migration.Gps (Gps)
import Beckn.Types.Core.Migration.Scalar (Scalar)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

-- FIXME: Gps is a string, it cannot be united with an object, possible error here
-- allOf union
-- allOf unions will be gone soon, so we're just waiting
data Circle = Circle
  { _gps :: Gps,
    _radius :: Scalar
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Circle
