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
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Intent = Intent
  { provider :: Maybe ProviderInfo,
    fulfillment :: Maybe FulFillmentInfo,
    payment :: Maybe Payment,
    category :: Maybe Category,
    offer :: Maybe Offer,
    item :: Maybe Item,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance Example Intent where
  example =
    Intent
      { provider = Nothing,
        fulfillment = example,
        payment = Nothing,
        category = Nothing,
        offer = Nothing,
        item = Nothing,
        tags = Nothing
      }

data ProviderInfo = ProviderInfo
  { id :: Maybe Text,
    descriptor :: Maybe DescriptorName,
    category_id :: Maybe Text,
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
    vehicle :: Maybe Vehicle
  }
  deriving (Generic, Show)

instance Example FulFillmentInfo where
  example =
    FulFillmentInfo
      { id = Nothing,
        start = example,
        end = example,
        tags = Nothing,
        vehicle = Nothing
      }

data LocationAndTime = LocationAndTime
  { location :: Maybe Location,
    time :: Maybe Time
  }
  deriving (Generic, Show)

instance Example LocationAndTime where
  example =
    LocationAndTime
      { location = example,
        time = Nothing
      }

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
