module Beckn.Types.Core.City where

import Data.Text
import EulerHS.Prelude

data City = City
  { name :: Text,
    code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
