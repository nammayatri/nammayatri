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
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider :: Maybe ProviderInfo,
    _fulfillment :: Maybe FulFillmentInfo,
    _payment :: Maybe Payment,
    _category :: Maybe Category,
    _offer :: Maybe Offer,
    _item :: Maybe Item,
    _purpose :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

data ProviderInfo = ProviderInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName,
    _locations :: Maybe [IdObject]
  }
  deriving (Generic, Show)

newtype DescriptorName = DescriptorName {_name :: Text}
  deriving (Generic, Show)

data FulFillmentInfo = FulFillmentInfo
  { _id :: Maybe Text,
    _start :: Maybe LocationAndTime,
    _end :: Maybe LocationAndTime,
    _tags :: Maybe Tags,
    _vehicle :: Vehicle
  }
  deriving (Generic, Show)

data LocationAndTime = LocationAndTime
  { _location :: Maybe Location,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ProviderInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON ProviderInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON FulFillmentInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON FulFillmentInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON LocationAndTime where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON LocationAndTime where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON DescriptorName where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON DescriptorName where
  toJSON = genericToJSON stripLensPrefixOptions
