module Types.Beckn.Task where

import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson hiding (Error)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State)
import Types.Beckn.Agent
import Types.Beckn.Descriptor
import Types.Beckn.Location
import Types.Beckn.Package
import Types.Beckn.Person
import Types.Beckn.State

data PickupOrDrop = PickupOrDrop
  { location :: Location,
    instructions :: Maybe [Descriptor],
    poc :: Person,
    time :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON PickupOrDrop where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PickupOrDrop where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example PickupOrDrop where
  example =
    PickupOrDrop
      { location = example,
        instructions = example,
        poc = example,
        time = Nothing
      }

-- To be used as State.descriptor.code
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
    { constructorTagModifier = replaceUnderscoresString
    }

instance ToJSON TaskState where
  toJSON = genericToJSON taskStateOptions

instance FromJSON TaskState where
  parseJSON = genericParseJSON taskStateOptions

data Task = Task
  { id :: Text,
    item_id :: Text,
    next_task_id :: Maybe Text,
    previous_task_id :: Maybe Text,
    state :: Maybe State,
    pickup :: PickupOrDrop,
    drop :: PickupOrDrop,
    _return :: PickupOrDrop,
    package :: Package,
    agent :: Maybe Agent,
    vehicle :: Maybe Vehicle,
    created_at :: Maybe UTCTime,
    updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Task where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Task where
  example =
    Task
      { id = idExample,
        item_id = idExample,
        next_task_id = Nothing,
        previous_task_id = Nothing,
        state = Nothing,
        pickup = example,
        drop = example,
        _return = example,
        package = example,
        agent = example,
        vehicle = example,
        created_at = example,
        updated_at = example
      }
