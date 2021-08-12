module Beckn.Types.Core.State where

import Beckn.Types.App (Value)
import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (State)

data State = State
  { descriptor :: Descriptor,
    updated_at :: Maybe UTCTime,
    updated_by :: Maybe Text,
    update_metadata :: Maybe Value
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
