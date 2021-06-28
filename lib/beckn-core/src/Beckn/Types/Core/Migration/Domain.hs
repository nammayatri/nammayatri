module Beckn.Types.Core.Migration.Domain (Domain (..)) where

import Beckn.Utils.JSON (replaceUnderscoresString)
import Data.Aeson
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  deriving (Eq, Generic, Show)

domainOptions :: Options
domainOptions =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString
    }

instance ToJSON Domain where
  toJSON = genericToJSON domainOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON domainOptions
