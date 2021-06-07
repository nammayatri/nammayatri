module Beckn.Types.Core.ItemQuantity where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (maximum, minimum)

data Quantity = Quantity
  { count :: Integer,
    quantity :: Scalar
  }
  deriving (Generic, Show)

instance FromJSON Quantity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Quantity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Quantity where
  example =
    Quantity
      { count = 2,
        quantity = example
      }

data ItemQuantity = ItemQuantity
  { allocated :: Maybe Quantity,
    available :: Maybe Quantity,
    maximum :: Maybe Quantity,
    minimum :: Maybe Quantity,
    selected :: Maybe Quantity
  }
  deriving (Generic, Show)

instance FromJSON ItemQuantity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ItemQuantity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example ItemQuantity where
  example =
    ItemQuantity
      { allocated = example,
        available = example,
        maximum = example,
        minimum = example,
        selected = example
      }
