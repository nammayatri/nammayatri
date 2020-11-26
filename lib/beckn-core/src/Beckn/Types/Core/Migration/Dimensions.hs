{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Dimensions where

import Beckn.Types.Core.Migration.Scalar (Scalar)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Dimensions = Dimensions
  { _length :: Maybe Scalar,
    _breadth :: Maybe Scalar,
    _height :: Maybe Scalar
  }
  deriving (Generic, Show)

deriveJSON ''Dimensions 'stripAllLensPrefixOptions
