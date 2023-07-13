{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CallbackRequest where

import Domain.Types.CallbackRequest
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption (Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.CallbackRequest as BeamCR

create :: L.MonadFlow m => CallbackRequest -> m (MeshResult ())
create callbackRequest = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamCR.CallbackRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainCallbackRequestToBeam callbackRequest)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

transformBeamCallbackRequestToDomain :: BeamCR.CallbackRequest -> CallbackRequest
transformBeamCallbackRequestToDomain BeamCR.CallbackRequestT {..} = do
  CallbackRequest
    { id = Id id,
      merchantId = Id merchantId,
      customerName = customerName,
      customerPhone = EncryptedHashed (Encrypted customerPhoneEncrypted) customerPhoneHash,
      customerMobileCountryCode = customerMobileCountryCode,
      status = status,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainCallbackRequestToBeam :: CallbackRequest -> BeamCR.CallbackRequest
transformDomainCallbackRequestToBeam CallbackRequest {..} =
  BeamCR.defaultCallbackRequest
    { BeamCR.id = getId id,
      BeamCR.merchantId = getId merchantId,
      BeamCR.customerName = customerName,
      BeamCR.customerPhoneEncrypted = customerPhone & unEncrypted . (.encrypted),
      BeamCR.customerPhoneHash = customerPhone & (.hash),
      BeamCR.customerMobileCountryCode = customerMobileCountryCode,
      BeamCR.status = status,
      BeamCR.createdAt = createdAt,
      BeamCR.updatedAt = updatedAt
    }
