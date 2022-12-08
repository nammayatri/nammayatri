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
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Maybe
import Data.OpenApi (ToSchema)
import Domain.Types.Merchant (MerchantAPIEntity)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data TranspAdminProfileRes = TranspAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: MerchantAPIEntity
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

getProfile :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => SP.Person -> m TranspAdminProfileRes
getProfile admin = do
  let merchantId = admin.merchantId
  org <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeTranspAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

updateProfile :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => SP.Person -> UpdateTranspAdminProfileReq -> m UpdateTranspAdminProfileRes
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
  return $ makeTranspAdminProfileRes personAPIEntity (DM.makeMerchantAPIEntity org)

makeTranspAdminProfileRes :: SP.PersonAPIEntity -> DM.MerchantAPIEntity -> TranspAdminProfileRes
makeTranspAdminProfileRes SP.PersonAPIEntity {..} org =
  TranspAdminProfileRes
    { organization = org,
      ..
    }
