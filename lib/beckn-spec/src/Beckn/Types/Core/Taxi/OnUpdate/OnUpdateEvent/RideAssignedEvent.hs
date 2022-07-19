module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_ASSIGNED))
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example, name, tags)
import GHC.Exts (fromList)

data RideAssignedEvent = RideAssignedEvent
  { id :: Text,
    state :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, Show)

instance ToJSON RideAssignedEvent where
  toJSON RideAssignedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "state" .= state
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_ASSIGNED) :: A.Object)))

instance FromJSON RideAssignedEvent where
  parseJSON = withObject "RideAssignedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_ASSIGNED) $ fail "Wrong update_type."
    RideAssignedEvent
      <$> obj .: "id"
      <*> obj .: "state"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"

instance ToSchema RideAssignedEvent where
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
      NamedSchema (Just "RideAssignedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("state", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment)
              ]
          & required
            L..~ [ "id",
                   "state",
                   "./komn/update_target",
                   "fulfillment"
                 ]

data FulfillmentInfo = FulfillmentInfo
  { id :: Text, -- bppRideId
    start :: StartInfo,
    agent :: Agent,
    vehicle :: Vehicle
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype StartInfo = StartInfo
  { authorization :: Authorization
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Authorization = Authorization
  { _type :: Text,
    token :: Text
  }
  deriving (Eq, Generic, Show)

instance ToSchema Authorization where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Authorization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Authorization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Agent = Agent
  { name :: Text,
    phone :: Text,
    rating :: Maybe DecimalValue,
    tags :: AgentTags
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ToSchema Agent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype AgentTags = AgentTags
  { registered_at :: UTCTime
  }
  deriving (Eq, Generic, Show)

instance ToSchema AgentTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions agentTagsJSONOptions

instance FromJSON AgentTags where
  parseJSON = genericParseJSON agentTagsJSONOptions

instance ToJSON AgentTags where
  toJSON = genericToJSON agentTagsJSONOptions

agentTagsJSONOptions :: A.Options
agentTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "registered_at" -> "./komn/registered_at"
        a -> a
    }

data Vehicle = Vehicle
  { model :: Text,
    variant :: Text,
    color :: Text,
    registration :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Vehicle where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
