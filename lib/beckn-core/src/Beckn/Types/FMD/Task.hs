module Beckn.Types.FMD.Task where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import Beckn.Types.FMD.Agent
import Beckn.Types.FMD.Package
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Common
import Data.Time (UTCTime)
import EulerHS.Prelude

data PickupOrDrop = PickupOrDrop
  { _location :: Location,
    _instructions :: [Descriptor],
    _poc :: Person,
    _time :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON PickupOrDrop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PickupOrDrop where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example PickupOrDrop where
  example =
    PickupOrDrop
      { _location = example,
        _instructions = example,
        _poc = example,
        _time = Nothing
      }

data Task = Task
  { _id :: Text,
    _item_id :: Text,
    _next_task_id :: Maybe Text,
    _previous_task_id :: Maybe Text,
    _state :: Text, -- "SEARCHINNG-FOR-FMD-AGENT", "ASSIGNED-AGENT", "EN-ROUTE-TO-PICKCUP", "AT-PICKUP-LOCATION", "PICKED-UP-PACKAGE", "EN-ROUTE-TO-DROP", "AT-DROP-LOCATION", "DROPPED-PACKAGE"
    _pickup :: PickupOrDrop,
    _drop :: PickupOrDrop,
    _package :: Package,
    _agent :: Maybe Agent,
    _vehicle :: Maybe Vehicle,
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Task where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Task where
  example =
    Task
      { _id = idExample,
        _item_id = idExample,
        _next_task_id = Nothing,
        _previous_task_id = Nothing,
        _state = "ASSIGNED-AGENT",
        _pickup = example,
        _drop = example,
        _package = example,
        _agent = example,
        _vehicle = example,
        _created_at = example,
        _updated_at = example
      }
