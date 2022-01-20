module Core.Fulfillment where

import Beckn.Prelude
import Core.Location
import Core.Time

data SearchFulfillment = SearchFulfillment
  { start :: FulfillmentLocation,
    end :: FulfillmentLocation
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype FulfillmentLocation = FulfillmentLocation
  { location :: LocationGps
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OnSearchFulfillment = OnSearchFulfillment
  { id :: Text,
    start :: FulfillmentLocationTime,
    end :: FulfillmentLocationTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FulfillmentLocationTime = FulfillmentLocationTime
  { location :: LocationId,
    time :: Time
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype FulfillmentId = FulfillmentId
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnConfirmFulfillment = OnSearchFulfillment

type OnStatusFulfillment = OnSearchFulfillment
