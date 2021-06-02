module Beckn.Types.Core.Migration.Intent (Intent (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Intent = Intent
  { query_string :: Maybe Text,
    provider :: Maybe ProviderInfo,
    fulfillment :: Maybe FulFillmentInfo,
    payment :: Maybe Payment,
    category :: Maybe Category,
    offer :: Maybe Offer,
    item :: Maybe Item,
    purpose :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

data ProviderInfo = ProviderInfo
  { id :: Maybe Text,
    descriptor :: Maybe DescriptorName,
    locations :: Maybe [IdObject]
  }
  deriving (Generic, Show)

newtype DescriptorName = DescriptorName {name :: Text}
  deriving (Generic, Show)

data FulFillmentInfo = FulFillmentInfo
  { id :: Maybe Text,
    start :: Maybe LocationAndTime,
    end :: Maybe LocationAndTime,
    tags :: Maybe Tags,
    vehicle :: Vehicle
  }
  deriving (Generic, Show)

data LocationAndTime = LocationAndTime
  { location :: Maybe Location,
    time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Intent where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON ProviderInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProviderInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON FulFillmentInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FulFillmentInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON LocationAndTime where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LocationAndTime where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON DescriptorName where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DescriptorName where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
