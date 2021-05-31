module Beckn.Types.Core.Migration.Fulfillment
  ( Fulfillment (..),
    FulfillmentParticipant (..),
    FulfillmentDetails (..),
  )
where

import Beckn.Types.Core.Migration.Contact (Contact)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Types.Core.Migration.State (State)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Data.Aeson (withObject, (.!=), (.:), (.:?))
import EulerHS.Prelude hiding (State)

data Fulfillment = Fulfillment
  { _id :: Maybe Text,
    _type :: Maybe Text,
    _state :: Maybe State,
    _tracking :: Bool,
    _customer :: Maybe FulfillmentParticipant,
    _agent :: Maybe FulfillmentParticipant,
    _vehicle :: Maybe Vehicle,
    _start :: Maybe FulfillmentDetails,
    _end :: Maybe FulfillmentDetails,
    _purpose :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .:? "id"
      <*> o .:? "type"
      <*> o .:? "state"
      <*> o .:? "tracking" .!= False
      <*> o .:? "customer"
      <*> o .:? "agent"
      <*> o .:? "vehicle"
      <*> o .:? "start"
      <*> o .:? "end"
      <*> o .:? "purpose"
      <*> o .: "tags"

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripLensPrefixOptions

data FulfillmentParticipant = FulfillmentParticipant
  { _person :: Maybe Person,
    _contact :: Maybe Contact
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentParticipant where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON FulfillmentParticipant where
  toJSON = genericToJSON stripLensPrefixOptions

data FulfillmentDetails = FulfillmentDetails
  { _location :: Maybe Location,
    _time :: Maybe Time,
    _instructions :: Maybe Descriptor,
    _contact :: Contact,
    _person :: Person
  }
  deriving (Generic, Show)

instance FromJSON FulfillmentDetails where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON FulfillmentDetails where
  toJSON = genericToJSON stripLensPrefixOptions
