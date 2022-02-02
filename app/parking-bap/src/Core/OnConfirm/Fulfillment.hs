module Core.OnConfirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Core.Common.Descriptor
import Core.Common.Gps
import Core.Common.Time
import Core.Common.Vehicle (Vehicle)
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

data Start = Start
  { location :: StartLocation,
    contact :: Contact,
    time :: Time
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data StartLocation = StartLocation
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Gps
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype End = End
  { time :: Time
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Contact = Contact
  { phone :: Text,
    email :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
