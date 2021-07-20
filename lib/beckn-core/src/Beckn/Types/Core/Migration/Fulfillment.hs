module Beckn.Types.Core.Migration.Fulfillment
  ( Fulfillment (..),
    FulfillmentParticipant (..),
    FulfillmentDetails (..),
  )
where

import Beckn.Types.Core.Migration.Agent (Agent)
import Beckn.Types.Core.Migration.Contact (Contact)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Types.Core.Migration.State (State)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson (withObject, (.!=), (.:?))
import EulerHS.Prelude hiding (State, id, state)

data Fulfillment = Fulfillment
  { id :: Maybe Text,
    _type :: Maybe Text,
    provider_id :: Maybe Text,
    state :: Maybe State,
    tracking :: Bool,
    customer :: Maybe FulfillmentParticipant,
    agent :: Maybe Agent,
    vehicle :: Maybe Vehicle,
    start :: Maybe FulfillmentDetails,
    end :: Maybe FulfillmentDetails,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .:? "id"
      <*> o .:? "type"
      <*> o .:? "provider_id"
      <*> o .:? "state"
      <*> o .:? "tracking" .!= False
      <*> o .:? "customer"
      <*> o .:? "agent"
      <*> o .:? "vehicle"
      <*> o .:? "start"
      <*> o .:? "end"
      <*> o .:? "tags"

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Fulfillment where
  example =
    Fulfillment
      { id = Nothing,
        _type = Nothing,
        provider_id = Nothing,
        state = Nothing,
        tracking = False,
        customer = Nothing,
        agent = Nothing,
        vehicle = Nothing,
        start = Nothing,
        end = Nothing,
        tags = Nothing
      }

data FulfillmentParticipant = FulfillmentParticipant
  { person :: Maybe Person,
    contact :: Maybe Contact
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentParticipant where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FulfillmentParticipant where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data FulfillmentDetails = FulfillmentDetails
  { location :: Maybe Location,
    time :: Maybe Time,
    instructions :: Maybe Descriptor,
    contact :: Maybe Contact,
    person :: Maybe Person
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FulfillmentDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
