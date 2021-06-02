module Beckn.Types.Core.Migration.AddOn (AddOn (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Price (Price)
import Beckn.Utils.JSON
import EulerHS.Prelude

data AddOn = AddOn
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price
  }
  deriving (Generic, Show)

instance FromJSON AddOn where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON AddOn where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
