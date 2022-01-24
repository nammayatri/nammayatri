module Core.Fulfillment where

import Core.Location
import Core.Time
import Data.Aeson
import Relude hiding (id)

data FullInfoFulfillment = FullInfoFulfillment
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
