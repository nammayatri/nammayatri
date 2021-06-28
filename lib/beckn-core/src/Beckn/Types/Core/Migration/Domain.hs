module Beckn.Types.Core.Migration.Domain (Domain (..)) where

import Beckn.Utils.JSON (constructorsWithHyphens)
import Data.Aeson
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  deriving (Eq, Generic, Show)

instance ToJSON Domain where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Domain where
  parseJSON = genericParseJSON constructorsWithHyphens
