module Domain.Action.UI.OrgAdmin
  ( OrgAdminProfileRes (..),
    UpdateOrgAdminProfileReq (..),
    UpdateOrgAdminProfileRes,
    getProfile,
    updateProfile,
  )
where

import Control.Applicative
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Kernel.External.Encryption (decrypt)
import Kernel.External.FCM.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data OrgAdminProfileRes = OrgAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOrgAdminProfileReq = UpdateOrgAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type UpdateOrgAdminProfileRes = OrgAdminProfileRes

getProfile :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => SP.Person -> m OrgAdminProfileRes
getProfile admin = do
  let merchantId = admin.merchantId
  org <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

updateProfile :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => SP.Person -> UpdateOrgAdminProfileReq -> m UpdateOrgAdminProfileRes
updateProfile admin req = do
  let merchantId = admin.merchantId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  Esq.runTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> DM.MerchantAPIEntity -> OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  OrgAdminProfileRes
    { organization = org,
      ..
    }
