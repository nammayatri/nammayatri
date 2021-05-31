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
import EulerHS.Prelude

data Provider = Provider
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _time :: Maybe Time,
    _categories :: Maybe [Category],
    _fulfillments :: Maybe [Fulfillment],
    _payments :: Maybe [Payment],
    _locations :: Maybe [LocationAndTime],
    _offers :: Maybe [Offer],
    _items :: Maybe [Item],
    _exp :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Provider where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Provider where
  toJSON = genericToJSON stripAllLensPrefixOptions

data LocationAndTime = LocationAndTime
  { _location :: Maybe Location,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

instance FromJSON LocationAndTime where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON LocationAndTime where
  toJSON = genericToJSON stripLensPrefixOptions
