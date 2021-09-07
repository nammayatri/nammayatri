module Types.API.OrgAdmin where

import Beckn.External.FCM.Types (FCMRecipientToken)
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import Types.Storage.Organization (OrganizationAPIEntity)
import qualified Types.Storage.Person as SP

data OrgAdminProfileRes = OrgAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: OrganizationAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data UpdateOrgAdminProfileReq = UpdateOrgAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type UpdateOrgAdminProfileRes = OrgAdminProfileRes
