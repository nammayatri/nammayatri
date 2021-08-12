module Types.API.Profile where

import Beckn.External.FCM.Types as FCM
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Person as SPerson
import Data.OpenApi (ToSchema)

type ProfileRes = SPerson.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
