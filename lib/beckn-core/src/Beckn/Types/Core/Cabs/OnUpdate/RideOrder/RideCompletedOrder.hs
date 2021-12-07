module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideCompletedOrder
  ( module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideCompletedOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.Common.Payment as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideOrderStatus (RideOrderStatus (COMPLETED))
import Beckn.Utils.Example
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, state, (.=))
import GHC.Exts (fromList)

data RideCompletedOrder = RideCompletedOrder
  { id :: Text,
    fulfillment :: RideCompletedFulfillment,
    payment :: Payment
  }
  deriving (Generic, Show)

instance ToJSON RideCompletedOrder where
  toJSON RideCompletedOrder {..} =
    A.Object $
      "id" .= id
        <> "fulfillment" .= fulfillment
        <> "payment" .= payment
        <> "state" .= COMPLETED

instance FromJSON RideCompletedOrder where
  parseJSON = withObject "RideCompletedOrder" $ \obj -> do
    state <- obj .: "state"
    unless (state == COMPLETED) $ fail "Wrong state."
    RideCompletedOrder
      <$> obj .: "id"
      <*> obj .: "fulfillment"
      <*> obj .: "payment"

instance ToSchema RideCompletedOrder where
  declareNamedSchema _ = do
    id <- declareSchemaRef (Proxy :: Proxy Text)
    fulfillment <- declareSchemaRef (Proxy :: Proxy RideCompletedFulfillment)
    payment <- declareSchemaRef (Proxy :: Proxy Payment)
    state <- declareSchemaRef (Proxy :: Proxy RideOrderStatus)
    return $
      NamedSchema (Just "RideCompletedOrder") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", id),
                ("fulfillment", fulfillment),
                ("payment", payment),
                ("state", state)
              ]
          & required L..~ ["id", "fulfillment", "payment", "state"]

instance Example RideCompletedOrder where
  example =
    RideCompletedOrder
      { id = "ride_booking_id",
        fulfillment = example,
        payment = example
      }

data RideCompletedFulfillment = RideCompletedFulfillment
  { id :: Text,
    chargeable_distance :: DecimalValue
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example RideCompletedFulfillment where
  example =
    RideCompletedFulfillment
      { id = "ride_id",
        chargeable_distance = 123
      }
