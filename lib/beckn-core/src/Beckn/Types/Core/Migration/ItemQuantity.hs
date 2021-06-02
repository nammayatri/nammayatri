module Beckn.Types.Core.Migration.ItemQuantity where

import Beckn.Types.Core.Migration.Scalar
import Beckn.Utils.JSON
import EulerHS.Prelude

data Quantity = Quantity
  { count :: Maybe Integer,
    measure :: Maybe Scalar
  }
  deriving (Generic, Show)

instance FromJSON Quantity where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Quantity where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

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
