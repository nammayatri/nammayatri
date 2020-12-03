module Beckn.Types.Core.Migration.AddOn (AddOn (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import EulerHS.Prelude

data AddOn = AddOn
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _price :: Maybe Price
  }
  deriving (Generic, Show)

instance FromJSON AddOn where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON AddOn where
  toJSON = genericToJSON stripAllLensPrefixOptions
