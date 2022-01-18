module Core1.Provider where

import Beckn.Prelude
import Core1.Descriptor
import Core1.Fulfillment
import Core1.Item
import Core1.Location

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
