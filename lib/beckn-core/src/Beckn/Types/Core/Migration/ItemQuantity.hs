{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.ItemQuantity where

import Beckn.Types.Core.Migration.Scalar
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Quantity = Quantity
  { _count :: Maybe Integer,
    _measure :: Maybe Scalar
  }
  deriving (Generic, Show)

deriveJSON ''Quantity 'stripAllLensPrefixOptions

data ItemQuantity = ItemQuantity
  { _allocated :: Maybe Quantity,
    _available :: Maybe Quantity,
    _maximum :: Maybe Quantity,
    _minimum :: Maybe Quantity,
    _selected :: Maybe Quantity
  }
  deriving (Generic, Show)

deriveJSON ''ItemQuantity 'stripAllLensPrefixOptions
