module Types.API.Profile where

import Beckn.External.FCM.Types as FCM
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as SPerson
import EulerHS.Prelude hiding (id)

type ProfileRes = SPerson.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
