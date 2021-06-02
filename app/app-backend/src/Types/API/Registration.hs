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

instance ToJSON InitiateLoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ReInitiateLoginReq = ReInitiateLoginReq
  { medium :: Medium,
    __type :: LoginType,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON ReInitiateLoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ReInitiateLoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data InitiateLoginRes = InitiateLoginRes
  { tokenId :: Text,
    attempts :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

---------- Verify Login --------
data LoginReq = LoginReq
  { medium :: Medium,
    __type :: LoginType,
    hash :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)
