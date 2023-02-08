module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_STARTED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (id, (.=))
import GHC.Exts (fromList)
import Kernel.Utils.Schema

data RideStartedEvent = RideStartedEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON RideStartedEvent where
  toJSON RideStartedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_STARTED) :: A.Object)))

instance FromJSON RideStartedEvent where
  parseJSON = withObject "RideStartedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_STARTED) $ fail "Wrong update_type."
    RideStartedEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"

instance ToSchema RideStartedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
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
      NamedSchema (Just "RideStartedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required L..~ ["id", "./komn/update_target", "fulfillment"]

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
