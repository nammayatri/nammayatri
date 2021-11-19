module Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideCompletedOrder
  ( module Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideCompletedOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.Common.Payment as Reexport
import Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (COMPLETED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import Data.Time
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data RideCompletedOrder = RideCompletedOrder
  { id :: Text,
    fulfillment :: RideCompletedFulfillment,
    payment :: Payment,
    updated_at :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedOrder where
  toJSON RideCompletedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "payment" .= payment
        <> "updated_at" .= updated_at
        <> "status" .= COMPLETED

instance FromJSON RideCompletedOrder where
  parseJSON = withObject "RideCompletedOrder" $ \obj -> do
    status <- obj .: "status"
    when (status == COMPLETED) $ fail "Wrong status."
    RideCompletedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"
      <*> obj .: "payment"
      <*> obj .: "updated_at"

instance ToSchema RideCompletedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy RideCompletedFulfillment)
    payment <- declareSchemaRef (Proxy :: Proxy Payment)
    updated_at <- declareSchemaRef (Proxy :: Proxy UTCTime)
    status <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "RideCompletedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("payment", payment),
                ("updated_at", updated_at),
                ("status", status)
              ]
          & required L..~ ["id", "fulfillment", "payment", "updated_at", "status"]

instance Example RideCompletedOrder where
  example =
    RideCompletedOrder
      { id = "ride_booking_id",
        fulfillment = example,
        payment = example,
        updated_at = example
      }

data RideCompletedFulfillment = RideCompletedFulfillment
  { id :: Text,
    chargeable_distance :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example RideCompletedFulfillment where
  example =
    RideCompletedFulfillment
      { id = "ride_id",
        chargeable_distance = 123
      }
