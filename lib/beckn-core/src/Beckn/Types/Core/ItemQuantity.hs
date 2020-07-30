module Beckn.Types.Core.ItemQuantity where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Common
import EulerHS.Prelude

data Quantity = Quantity
  { _count :: Integer,
    _amount :: Scalar
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
        _amount = example
      }

data ItemQuantity = ItemQuantity
  { _allocated :: Quantity,
    _available :: Quantity,
    _maximum :: Quantity,
    _minimum :: Quantity,
    _selected :: Quantity
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
