module Beckn.Types.Core.Migration.Domain (Domain (..)) where

-- import Beckn.Utils.JSON (constructorsWithHyphens)
import Data.Aeson
import EulerHS.Prelude

newtype Domain = Domain Text
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
