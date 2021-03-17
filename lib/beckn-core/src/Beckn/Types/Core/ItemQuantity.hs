module Beckn.Types.Core.ItemQuantity where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import EulerHS.Prelude

data Quantity = Quantity
  { _count :: Integer,
    _quantity :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Quantity where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Quantity where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Quantity where
  example =
    Quantity
      { _count = 2,
        _quantity = example
      }

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

instance Example ItemQuantity where
  example =
    ItemQuantity
      { _allocated = example,
        _available = example,
        _maximum = example,
        _minimum = example,
        _selected = example
      }
