module Beckn.Types.Core.State where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson (Value)
import Data.Time
import EulerHS.Prelude hiding (State)

data State = State
  { descriptor :: Descriptor,
    updated_at :: Maybe UTCTime,
    updated_by :: Maybe Text,
    update_metadata :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON State where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON State where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example State where
  example =
    State
      { descriptor = example,
        updated_at = example,
        updated_by = Nothing,
        update_metadata = Nothing
      }

withDescriptor :: Descriptor -> State
withDescriptor descriptor =
  State
    { descriptor = descriptor,
      updated_at = Nothing,
      updated_by = Nothing,
      update_metadata = Nothing
    }
