module Core.Search.Fulfillment where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps

data Fulfillment = Fulfillment
  { start :: FulfillmentDetails,
    end :: FulfillmentDetails
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype FulfillmentDetails = FulfillmentDetails
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Location = Location {gps :: Gps}
  deriving (Generic, Show, ToJSON, FromJSON)
