module Beckn.Types.Core.Migration.State (State (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State)

data State = State
  { descriptor :: Maybe Descriptor,
    updated_at :: Maybe UTCTime,
    updated_by :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
