module Beckn.Types.Core.Migration.Tags (Tags (..)) where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Tags = Tags (HashMap Text Text)
  deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)
