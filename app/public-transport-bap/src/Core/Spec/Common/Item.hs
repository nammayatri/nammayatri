module Core.Spec.Common.Item where

import Beckn.Prelude

data Item = Item
  { departure_id :: Text,
    fare_id :: Text,
    canBook :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DepartureTime = DepartureTime
  { timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype ArrivalTime = ArrivalTime
  { timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)