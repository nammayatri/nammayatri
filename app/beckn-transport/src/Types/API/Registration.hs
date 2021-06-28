module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Predicate
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Beckn.Utils.JSON
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import EulerHS.Prelude

data InitiateLoginReq = InitiateLoginReq
  { medium :: Medium,
    __type :: LoginType,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    role :: Role,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic)

instance FromJSON InitiateLoginReq where
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "InitiateLoginReq" validateInitiateLoginReq

validateInitiateLoginReq :: Validate InitiateLoginReq
validateInitiateLoginReq InitiateLoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode
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
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "ReInitiateLoginReq" validateReInitiateLoginReq

validateReInitiateLoginReq :: Validate ReInitiateLoginReq
validateReInitiateLoginReq ReInitiateLoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode
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
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "LoginReq" validateLoginReq

validateLoginReq :: Validate LoginReq
validateLoginReq LoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validate "hash" hash $ ExactLength 4 `And` star P.digit
    ]

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Maybe DecryptedPerson
  }
  deriving (Generic, ToJSON)
