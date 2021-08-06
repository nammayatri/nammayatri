module Beckn.Types.Core.Migration.Language where

import EulerHS.Prelude

newtype Language = Language
  { code :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
