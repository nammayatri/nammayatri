module Beckn.Types.Core.Domain where

import Data.Aeson
import qualified Data.Text as T
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
    { constructorTagModifier = T.unpack . T.replace "_" "-" . T.pack
    }

instance ToJSON Domain where
  toJSON = genericToJSON domainOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON domainOptions
