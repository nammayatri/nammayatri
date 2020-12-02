{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Fulfillment (Fulfillment (..), Agent (..), FulfillmentDetails (..)) where

import Beckn.Types.Core.Migration.Contact (Contact)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Types.Core.Migration.State (State)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import Beckn.Types.Core.Migration.Vehicle (Vehicle)
import Beckn.Utils.JSON (deriveJSON, uniteObjects)
import EulerHS.Prelude hiding (State)

data Fulfillment = Fulfillment
  { _id :: Maybe Text,
    _type :: Maybe Text,
    _state :: Maybe State,
    _tracking :: Maybe Bool,
    _agent :: Maybe Agent,
    _vehicle :: Maybe Vehicle,
    _start :: Maybe FulfillmentDetails,
    _end :: Maybe FulfillmentDetails,
    _purpose :: Maybe Text,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

-- allOf union
data Agent = Agent Person [Contact]
  deriving (Generic, Show)

instance FromJSON Agent where
  parseJSON v = Agent <$> parseJSON v <*> parseJSON v

instance ToJSON Agent where
  toJSON (Agent p cs) = uniteObjects [toJSON p, toJSON cs]

data FulfillmentDetails = FulfillmentDetails
  { _location :: Maybe Location,
    _time :: Maybe Time,
    _instructions :: Maybe Descriptor,
    _contact :: [Contact]
  }
  deriving (Generic, Show)

deriveJSON ''Fulfillment 'stripLensPrefixOptions
deriveJSON ''FulfillmentDetails 'stripLensPrefixOptions
