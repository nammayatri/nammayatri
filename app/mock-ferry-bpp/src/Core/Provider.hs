module Core.Provider where

import Beckn.Prelude
import Core.Descriptor
import Core.Fulfillment
import Core.Item
import Core.Location

data Provider = Provider
  { id :: Text,
    descriptor :: DescriptorId,
    fulfillments :: [OnSearchFulfillment],
    locations :: [LocationDetails],
    items :: [OnSearchItem]
  }
  deriving (Generic, FromJSON, Show, ToJSON)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
