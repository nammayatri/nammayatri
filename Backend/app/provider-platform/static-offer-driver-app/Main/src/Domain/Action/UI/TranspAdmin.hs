{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.TranspAdmin
  ( TranspAdminProfileRes (..),
    UpdateTranspAdminProfileReq (..),
    UpdateTranspAdminProfileRes,
    getProfile,
    updateProfile,
  )
where

import Data.Maybe
import Data.OpenApi (ToSchema)
import Domain.Types.Merchant (MerchantAPIEntity)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
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

updateProfile :: forall m r. (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => SP.Person -> UpdateTranspAdminProfileReq -> m UpdateTranspAdminProfileRes
updateProfile admin req = do
  let merchantId = admin.merchantId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  Esq.runTransaction $
    QPerson.updatePersonRec @m updAdmin.id updAdmin
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
