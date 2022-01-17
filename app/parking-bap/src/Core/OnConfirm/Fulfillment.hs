module Core.OnConfirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnConfirm.End
import Core.OnConfirm.Start
import Core.OnConfirm.Vehicle (Vehicle)
import Data.OpenApi (ToSchema (..), fromAesonOptions)

data Fulfillment = Fulfillment
  { _type :: Text,
    tracking :: Bool,
    start :: Start,
    end :: End,
    vehicle :: Vehicle
  }
  deriving (Generic)

instance ToSchema Fulfillment where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Fulfillment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
