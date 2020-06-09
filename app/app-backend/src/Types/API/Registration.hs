module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Common
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data InitiateLoginReq = InitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _identifier :: Text,
    _role :: Maybe Role,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ReInitiateLoginReq = ReInitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _identifier :: Text,
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
    _identifier :: Text,
    _deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, ToSchema)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Person
  }
  deriving (Generic, ToJSON, ToSchema)
