module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent,
    module Reexport,
  )
where

import Beckn.Prelude (UTCTime)
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (DRIVER_ARRIVED))
import Beckn.Utils.Schema
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, title, value)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)

data DriverArrivedEvent = DriverArrivedEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo,
    arrival_time :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance ToJSON DriverArrivedEvent where
  toJSON DriverArrivedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= DRIVER_ARRIVED) :: A.Object)))
        <> "arrival_time" .= arrival_time

instance FromJSON DriverArrivedEvent where
  parseJSON = withObject "DriverArrivedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == DRIVER_ARRIVED) $ fail "Wrong update_type."
    DriverArrivedEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"
      <*> obj .: "arrival_time"

instance ToSchema DriverArrivedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    arrival_time <- declareSchemaRef (Proxy :: Proxy (Maybe UTCTime))
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type)]
            & required L..~ ["code"]
        fulfillment =
          toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
            & properties
              L.<>~ fromList [("state", Inline st)]
            & required L.<>~ ["state"]
    return $
      NamedSchema (Just "DriverArrivedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment),
                ("arrival_time", arrival_time)
              ]
          & required
            L..~ [ "id",
                   "./komn/update_target",
                   "fulfillment",
                   "arrival_time"
                 ]

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
