module Beckn.Types.Core.Migration.Domain (Domain (..)) where

import EulerHS.Prelude

newtype Domain = Domain Text
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
