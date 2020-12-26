module Beckn.Types.Core.Migration.Operator where

import Beckn.Types.Core.Migration.Person (Person)
import Beckn.Utils.JSON (uniteObjects)
import EulerHS.Prelude

-- allOf case
data Operator = Operator Person Experience'

newtype Experience' = Experience' {experience :: Experience}
  deriving (Generic, Show, ToJSON, FromJSON)

data Experience = Experience
  { label :: Maybe Text,
    value :: Maybe Text,
    unit :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance FromJSON Operator where
  parseJSON v = Operator <$> parseJSON v <*> parseJSON v

instance ToJSON Operator where
  toJSON (Operator p e) = uniteObjects [toJSON p, toJSON e]
