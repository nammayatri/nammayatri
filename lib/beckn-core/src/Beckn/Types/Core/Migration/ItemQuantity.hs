module Beckn.Types.Core.Migration.ItemQuantity where

import Beckn.Types.Core.Migration.Scalar
import EulerHS.Prelude

data Quantity = Quantity
  { _count :: Maybe Integer,
    _measure :: Maybe Scalar
  }
  deriving (Generic, Show)

instance FromJSON Quantity where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Quantity where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ItemQuantity = ItemQuantity
  { _allocated :: Maybe Quantity,
    _available :: Maybe Quantity,
    _maximum :: Maybe Quantity,
    _minimum :: Maybe Quantity,
    _selected :: Maybe Quantity
  }
  deriving (Generic, Show)

instance FromJSON ItemQuantity where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ItemQuantity where
  toJSON = genericToJSON stripAllLensPrefixOptions
