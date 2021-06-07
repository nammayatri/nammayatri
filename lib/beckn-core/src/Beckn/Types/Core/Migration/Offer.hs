module Beckn.Types.Core.Migration.Offer (Offer (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Offer = Offer
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    location_ids :: Maybe [Text],
    category_ids :: Maybe [Text],
    item_ids :: Maybe [Text],
    time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON Offer where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Offer where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
