module Beckn.Types.Core.Metro.OnSearch.Fulfillment where

import Beckn.Types.Core.Metro.OnSearch.Location (Location)
import Beckn.Types.Core.Metro.OnSearch.Time (Time)
import Beckn.Types.Core.Metro.OnSearch.Vehicle (Vehicle)
import Kernel.Prelude

data Fulfillment = Fulfillment
  { id :: Text,
    start :: FulfillmentDetails,
    end :: FulfillmentDetails,
    vehicle :: Vehicle
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FulfillmentDetails = FulfillmentDetails
  { location :: Location,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
