module Domain.Action.Dashboard.Management.VehicleInfo
  ( getVehicleInfoList,
    postVehicleInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo as Common
import Dashboard.Common.Driver (VehicleRegistrationCertificate)
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (generateGUID)
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (throwError)
import Storage.Queries.VehicleInfo (create, deleteAllByRcId, findAllByRcId)

getVehicleInfoList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ID.Id VehicleRegistrationCertificate ->
  Environment.Flow [Common.VehicleInfoAPIEntity]
getVehicleInfoList _merchantShortId _opCity vrcId = do
  vehicleInfoList <- findAllByRcId $ ID.cast vrcId
  pure $ convertVehicleInfoToVehicleInfoAPIEntity <$> vehicleInfoList
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} =
      Common.VehicleInfoAPIEntity
        { id = ID.cast id,
          rcId = ID.cast rcId,
          ..
        }

postVehicleInfoUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postVehicleInfoUpdate _merchantShortId _opCity req = do
  vrcId <- case req.newInfo of
    [] -> throwError $ InvalidRequest "VehicleInfo list is empty"
    (vi : _) -> pure $ ID.cast vi.rcId
  deleteAllByRcId vrcId
  mapM_ updatedVehicleInfo req.newInfo
  pure Kernel.Types.APISuccess.Success
  where
    updatedVehicleInfo Common.VehicleInfoAPIEntity {..} = do
      newId <- generateGUID
      create $
        DVI.VehicleInfo
          { id = newId,
            rcId = ID.cast rcId,
            ..
          }
