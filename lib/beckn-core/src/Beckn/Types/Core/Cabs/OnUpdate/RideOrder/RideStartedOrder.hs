module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideStartedOrder where

import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (STARTED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data RideStartedOrder = RideStartedOrder
  { id :: Text,
    fulfillment :: RideStartedFulfillment
  }
  deriving (Generic, Show)

instance ToJSON RideStartedOrder where
  toJSON RideStartedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "state" .= STARTED

instance FromJSON RideStartedOrder where
  parseJSON = withObject "RideStartedOrder" $ \obj -> do
    state <- obj .: "state"
    unless (state == STARTED) $ fail "Wrong state."
    RideStartedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"

instance ToSchema RideStartedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy RideStartedFulfillment)
    state <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "RideStartedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("state", state)
              ]
          & required L..~ ["id", "fulfillment", "updated_at", "state"]

instance Example RideStartedOrder where
  example =
    RideStartedOrder
      { id = "ride_booking_id",
        fulfillment = example
      }

newtype RideStartedFulfillment = RideStartedFulfillment
  { id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example RideStartedFulfillment where
  example =
    RideStartedFulfillment
      { id = "ride_id"
      }
