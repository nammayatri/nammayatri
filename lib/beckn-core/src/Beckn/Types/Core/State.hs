module Beckn.Types.Core.State where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Common
import Data.Aeson
import Data.Text
import Data.Time.LocalTime
import EulerHS.Prelude hiding (State)

data State = State
  { descriptor :: Descriptor,
    updated_at :: Maybe LocalTime,
    updated_by :: Maybe Text,
    update_metadata :: Maybe Value
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example State where
  example =
    State
      { descriptor = example,
        updated_at = Nothing,
        updated_by = Nothing,
        update_metadata = Nothing
      }
