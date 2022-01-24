module Core.OnSearch.Fulfillment where

import Beckn.Prelude
import Core.Location
import Core.Time

data Fulfillment = Fulfillment
  { id :: Text,
    start :: FulfillmentDetails,
    end :: FulfillmentDetails
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FulfillmentDetails = FulfillmentDetails
  { location :: LocationId,
    time :: Time
  }
  deriving (Generic, Show, FromJSON, ToJSON)
