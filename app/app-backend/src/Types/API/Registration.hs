module Types.API.Registration where

import Beckn.External.FCM.Types
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id)
import Types.Storage.Person (PersonAPIEntity)
import Types.Storage.RegistrationToken (RegistrationToken)

data AuthReq = AuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

validateAuthReq :: Validate AuthReq
validateAuthReq AuthReq {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

data AuthRes = AuthRes
  { authId :: Id RegistrationToken,
    attempts :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

type ResendAuthRes = AuthRes

---------- Verify Login --------
data AuthVerifyReq = AuthVerifyReq
  { otp :: Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, FromJSON, ToJSON, Show)

validateAuthVerifyReq :: Validate AuthVerifyReq
validateAuthVerifyReq AuthVerifyReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

data AuthVerifyRes = AuthVerifyRes
  { token :: Text,
    person :: PersonAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show)
