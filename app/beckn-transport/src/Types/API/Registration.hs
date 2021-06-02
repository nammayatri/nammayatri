module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Beckn.Utils.JSON
import EulerHS.Prelude

data InitiateLoginReq = InitiateLoginReq
  { medium :: Medium,
    __type :: LoginType,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    role :: Maybe Role,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

data ReInitiateLoginReq = ReInitiateLoginReq
  { medium :: Medium,
    __type :: LoginType,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON ReInitiateLoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

data InitiateLoginRes = InitiateLoginRes
  { tokenId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON)

---------- Verify Login --------
data LoginReq = LoginReq
  { medium :: Medium,
    __type :: LoginType,
    hash :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Maybe Person
  }
  deriving (Generic, ToJSON)
