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
    role :: Person.Role,
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

validateReInitiateLoginReq :: Validate ReInitiateLoginReq
validateReInitiateLoginReq ReInitiateLoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

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

validateLoginReq :: Validate LoginReq
validateLoginReq LoginReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validateField "hash" hash $ ExactLength 4 `And` star P.digit
    ]

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: UserInfoRes
  }
  deriving (Generic, ToJSON)

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
