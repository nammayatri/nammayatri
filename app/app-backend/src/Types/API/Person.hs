module Types.API.Person where

import Beckn.External.FCM.Types as FCM
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Person as SPerson

data GetPersonDetailsRes = GetPersonDetailsRes
  { id :: Id SPerson.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    role :: SPerson.Role,
    gender :: SPerson.Gender,
    email :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON)

data UpdateReq = UpdateReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    fullName :: Maybe Text,
    gender :: Maybe SPerson.Gender,
    email :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken
  }
  deriving (Generic, ToJSON, FromJSON)
