module Domain.Action.UI.TranspAdmin
  ( TranspAdminProfileRes (..),
    UpdateTranspAdminProfileReq (..),
    UpdateTranspAdminProfileRes,
    getProfile,
    updateProfile,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.External.FCM.Types (FCMRecipientToken)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Maybe
import Data.OpenApi (ToSchema)
import Domain.Types.Organization (OrganizationAPIEntity)
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data TranspAdminProfileRes = TranspAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: OrganizationAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateTranspAdminProfileReq = UpdateTranspAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type UpdateTranspAdminProfileRes = TranspAdminProfileRes

getProfile :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> m TranspAdminProfileRes
getProfile admin = do
  let Just orgId = admin.organizationId
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeTranspAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

updateProfile :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> UpdateTranspAdminProfileReq -> m UpdateTranspAdminProfileRes
updateProfile admin req = do
  let Just orgId = admin.organizationId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  Esq.runTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeTranspAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

makeTranspAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> TranspAdminProfileRes
makeTranspAdminProfileRes SP.PersonAPIEntity {..} org =
  TranspAdminProfileRes
    { organization = org,
      ..
    }
