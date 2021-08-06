module Beckn.Types.Core.Migration.Organization (Organization (..)) where

import EulerHS.Prelude

data Organization = Organization
  { name :: Maybe Text,
    cred :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
