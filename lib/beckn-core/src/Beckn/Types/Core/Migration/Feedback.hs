module Beckn.Types.Core.Migration.Feedback (Feedback) where

import EulerHS.Prelude hiding (id)

data Feedback = Feedback
  { id :: Maybe Text,
    descriptor :: Maybe Text,
    parent_id :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
