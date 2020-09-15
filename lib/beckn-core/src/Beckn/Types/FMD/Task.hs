module Beckn.Types.FMD.Task where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import Beckn.Types.FMD.Agent
import Beckn.Types.FMD.Package
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Common
import Data.Aeson hiding (Error)
import qualified Data.Text as T
import Data.Time (UTCTime)
import EulerHS.Prelude

data PickupOrDrop = PickupOrDrop
  { _location :: Location,
    _instructions :: Maybe [Descriptor],
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

data TaskState
  = SEARCHING_FOR_FMD_AGENT
  | ASSIGNED_AGENT
  | EN_ROUTE_TO_PICKUP
  | AT_PICKUP_LOCATION
  | PICKED_UP_PACKAGE
  | EN_ROUTE_TO_DROP
  | AT_DROP_LOCATION
  | DROPPED_PACKAGE
  | RETURN_INITIATED
  | EN_ROUTE_TO_RETURN_LOCATION
  | AT_RETURN_LOCATION
  | RETURNED_PACKAGE
  deriving (Show, Generic)

taskStateOptions :: Options
taskStateOptions =
  defaultOptions
    { constructorTagModifier = T.unpack . T.replace "_" "-" . T.pack
    }

instance ToJSON TaskState where
  toJSON = genericToJSON taskStateOptions

instance FromJSON TaskState where
  parseJSON = genericParseJSON taskStateOptions

data Task = Task
  { _id :: Text,
    _item_id :: Text,
    _next_task_id :: Maybe Text,
    _previous_task_id :: Maybe Text,
    _state :: Maybe TaskState,
    _pickup :: PickupOrDrop,
    _drop :: PickupOrDrop,
    _return :: PickupOrDrop,
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
        _state = Nothing,
        _pickup = example,
        _drop = example,
        _return = example,
        _package = example,
        _agent = example,
        _vehicle = example,
        _created_at = example,
        _updated_at = example
      }
