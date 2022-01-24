module Core.Provider where

import Beckn.Prelude
import Core.Descriptor
import Core.Location
import Core.OnSearch.Fulfillment
import Core.OnSearch.Item

data Provider = Provider
  { id :: Text,
    descriptor :: DescriptorId,
    fulfillments :: [Fulfillment],
    locations :: [LocationDetails],
    items :: [Item]
  }
  deriving (Generic, FromJSON, Show, ToJSON)

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON)
