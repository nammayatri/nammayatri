module Core.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Core.End
import Core.Start
import Core.Vehicle (Vehicle)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)

data Fulfillment = Fulfillment
  { _type :: Text,
    tracking :: Bool,
    start :: Start,
    end :: End,
    vehicle :: Vehicle
  }
  deriving (Generic)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
