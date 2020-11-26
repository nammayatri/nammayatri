{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.ProviderCatalog where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Item (Item)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Utils.JSON (deriveJSON, uniteObjects)
import Data.Time (UTCTime)
import EulerHS.Prelude

data ProviderCatalog = ProviderCatalog
  { _categories :: Maybe [Category],
    _fulfillments :: Maybe [Fulfillment],
    _payments :: Maybe [Payment],
    _locations :: Maybe [TimedLocation],
    _offers :: Maybe [Offer],
    _items :: Maybe [Item],
    _exp :: Maybe UTCTime
  }
  deriving (Generic)

-- allOf case
data TimedLocation = TimedLocation Location Time

instance FromJSON TimedLocation where
  parseJSON v = TimedLocation <$> parseJSON v <*> parseJSON v

instance ToJSON TimedLocation where
  toJSON (TimedLocation l t) = uniteObjects [toJSON l, toJSON t]

deriveJSON ''ProviderCatalog 'stripLensPrefixOptions
