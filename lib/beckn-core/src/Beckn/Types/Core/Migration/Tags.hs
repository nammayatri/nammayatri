module Beckn.Types.Core.Migration.Tags (Tags (..)) where

import EulerHS.Prelude

newtype Tags = Tags (HashMap Text Text)
  deriving (Generic, Show, FromJSON, ToJSON)
