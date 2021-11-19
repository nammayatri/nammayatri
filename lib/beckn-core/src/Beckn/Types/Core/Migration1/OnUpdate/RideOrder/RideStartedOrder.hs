module Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideStartedOrder where

import Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (STARTED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import Data.Time
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideStartedOrder = RideStartedOrder
  { id :: Text,
    fulfillment :: RideStartedFulfillment,
    updated_at :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON RideStartedOrder where
  toJSON RideStartedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "updated_at" .= updated_at
        <> "status" .= STARTED

instance FromJSON RideStartedOrder where
  parseJSON = withObject "RideStartedOrder" $ \obj -> do
    status <- obj .: "status"
    when (status == STARTED) $ fail "Wrong status."
    RideStartedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"
      <*> obj .: "updated_at"

instance ToSchema RideStartedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy RideStartedFulfillment)
    updated_at <- declareSchemaRef (Proxy :: Proxy UTCTime)
    status <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "RideStartedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("updated_at", updated_at),
                ("status", status)
              ]
          & required L..~ ["id", "fulfillment", "updated_at", "status"]

instance Example RideStartedOrder where
  example =
    RideStartedOrder
      { id = "ride_booking_id",
        fulfillment = example,
        updated_at = example
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