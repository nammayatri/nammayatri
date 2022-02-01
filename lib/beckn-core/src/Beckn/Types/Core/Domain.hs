module Beckn.Types.Core.Domain (Domain (..)) where

import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Example
import Beckn.Utils.JSON (replaceUnderscoresString)
import Data.Aeson
import Data.OpenApi hiding (Example)
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | PARKING
  | PUBLIC_TRANSPORT
  deriving (Eq, Generic, Show, Read, FromDhall)

instance Example Domain where
  example = MOBILITY

customAesonOptions :: Options
customAesonOptions =
  defaultOptions
    { constructorTagModifier = \case
        "MOBILITY" -> "nic2004:60221"
        "LOCAL_RETAIL" -> "nic2004:52110"
        "FINAL_MILE_DELIVERY" -> "nic2004:55204"
        "METRO" -> "nic2004:60212"
        "PARKING" -> "nic2004:63031"
        "PUBLIC_TRANSPORT" -> "nic2004:63032"
        val -> replaceUnderscoresString val, -- TODO: update remaining domains with codes
      sumEncoding = UntaggedValue
    }

instance ToSchema Domain where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions customAesonOptions

instance ToJSON Domain where
  toJSON = genericToJSON customAesonOptions

instance FromJSON Domain where
  parseJSON = genericParseJSON customAesonOptions
