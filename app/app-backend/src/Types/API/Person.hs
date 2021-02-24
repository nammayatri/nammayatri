module Types.API.Person where

import Beckn.External.FCM.Types as FCM
import qualified Beckn.Types.Storage.Person as SPerson
import EulerHS.Prelude

data GetPersonDetailsRes = GetPersonDetailsRes
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: SPerson.Role,
    gender :: SPerson.Gender
  }
  deriving (Generic, ToJSON, FromJSON)

data UpdateReq = UpdateReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    gender :: Maybe SPerson.Gender,
    deviceToken :: Maybe FCM.FCMRecipientToken
  }
  deriving (Generic, ToJSON, FromJSON)
