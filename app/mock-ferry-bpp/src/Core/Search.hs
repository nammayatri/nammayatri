module Core.Search where

import Core.Location
import Data.Aeson
import Relude

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Intent = Intent
  { fulfillment :: Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Fulfillment = Fulfillment
  { start :: FulfillmentLocation,
    end :: FulfillmentLocation
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype FulfillmentLocation = FulfillmentLocation
  { location :: LocationGps
  }
  deriving (Generic, Show, ToJSON, FromJSON)
