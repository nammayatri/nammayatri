module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Storage.Person
import Beckn.Types.Storage.RegistrationToken
import Beckn.Types.Validation.Predicate
import qualified Beckn.Types.Validation.Regex as R
import Beckn.Utils.JSON
import Beckn.Utils.Validation
import qualified Beckn.Utils.ValidationPredicates as P
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
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "InitiateLoginReq" validateInitiateLoginReq

validateInitiateLoginReq :: Validate InitiateLoginReq
validateInitiateLoginReq InitiateLoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode
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
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "ReInitiateLoginReq" validateReInitiateLoginReq

validateReInitiateLoginReq :: Validate ReInitiateLoginReq
validateReInitiateLoginReq ReInitiateLoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode
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
  parseJSON =
    genericParseJSON stripPrefixUnderscoreIfAny
      >=> runValidationFromJson "LoginReq" validateLoginReq

validateLoginReq :: Validate LoginReq
validateLoginReq LoginReq {..} =
  sequenceA_
    [ validate "mobileNumber" mobileNumber P.mobileNumber,
      validate "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validate "hash" hash $ ExactLength 4 `And` R.Many (R.Ch R.digit)
    ]

instance ToJSON LoginReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LoginRes = LoginRes
  { registrationToken :: Text,
    user :: Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)
