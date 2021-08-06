module Beckn.Types.Core.Migration.Policy where

import Beckn.Types.Core.Migration.Descriptor
import Beckn.Types.Core.Migration.Time
import EulerHS.Prelude hiding (id)

data Policy = Policy
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    parent_policy_id :: Maybe Text,
    time :: Maybe Time
  }
  deriving (Generic, FromJSON, ToJSON, Show)
