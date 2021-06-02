module Beckn.Types.Core.Migration.Provider (Provider (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Provider = Provider
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    time :: Maybe Time,
    categories :: Maybe [Category],
    fulfillments :: Maybe [Fulfillment],
    payments :: Maybe [Payment],
    locations :: Maybe [LocationAndTime],
    offers :: Maybe [Offer],
    items :: Maybe [Item],
    exp :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Provider where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Provider where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LocationAndTime = LocationAndTime
  { location :: Maybe Location,
    time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON LocationAndTime where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LocationAndTime where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
