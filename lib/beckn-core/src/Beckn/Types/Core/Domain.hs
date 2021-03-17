module Beckn.Types.Core.Domain where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  deriving (Eq, Generic, Show)

domainOptions :: Options
domainOptions =
  defaultOptions
    { constructorTagModifier = replaceUnderscoresString
    }

instance Example Domain where
  example = MOBILITY -- TODO :: add randomized values maybe?

instance ToJSON Domain where
  toJSON = genericToJSON domainOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON domainOptions
