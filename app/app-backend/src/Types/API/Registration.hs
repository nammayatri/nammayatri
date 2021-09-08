module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.JSON
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Person
import Types.Storage.RegistrationToken

data InitiateLoginReq = InitiateLoginReq
  { medium :: Medium,
    __type :: LoginType,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

validateInitiateLoginReq :: Validate InitiateLoginReq
validateInitiateLoginReq InitiateLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

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

validateReInitiateLoginReq :: Validate ReInitiateLoginReq
validateReInitiateLoginReq ReInitiateLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

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

validateLoginReq :: Validate LoginReq
validateLoginReq LoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validateField "hash" hash $ ExactLength 4 `And` star P.digit
    ]

instance ToJSON LoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: UserInfoRes
  }
  deriving (Generic, FromJSON, ToJSON)

data UserInfoRes = UserInfoRes
  { id :: Id Person.Person,
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: Person.Role,
    mobileNumber :: Maybe Text,
    organizationId :: Maybe (Id Org.Organization),
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON)

makeUserInfoRes :: Person.DecryptedPerson -> UserInfoRes
makeUserInfoRes Person.Person {..} = UserInfoRes {..}
