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

data ReInitiateLoginReq = ReInitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _mobileCountryCode :: Text,
    _mobileNumber :: Text,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON ReInitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data InitiateLoginRes = InitiateLoginRes
  { tokenId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema)

---------- Verify Login --------
data LoginReq = LoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _hash :: Text,
    _mobileCountryCode :: Text,
    _mobileNumber :: Text,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Maybe Person
  }
  deriving (Generic, ToJSON, ToSchema)
