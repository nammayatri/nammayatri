{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Circle (Circle (..)) where

import Beckn.Types.Core.Migration.GPS (GPS)
import Beckn.Types.Core.Migration.Scalar (Scalar)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

-- FIXME: Gps is a string, it cannot be united with an object, possible error here
-- allOf union
data Circle = Circle
  { _gps :: GPS,
    _radius :: Scalar
  }
  deriving (Generic, Show)

deriveJSON ''Circle 'stripAllLensPrefixOptions
