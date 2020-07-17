module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Data.Swagger
import EulerHS.Prelude

data InitiateLoginReq = InitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _mobileNumber :: Text,
    _mobileCountryCode :: Text,
    _role :: Maybe Role,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON InitiateLoginReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ReInitiateLoginReq = ReInitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _mobileNumber :: Text,
    _mobileCountryCode :: Text,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON ReInitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ReInitiateLoginReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data InitiateLoginRes = InitiateLoginRes
  { tokenId :: Text,
    attempts :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

---------- Verify Login --------
data LoginReq = LoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _hash :: Text,
    _mobileNumber :: Text,
    _mobileCountryCode :: Text,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LoginReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Person
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)
