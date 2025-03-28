module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
    postVehicleInfoUpdate,
  )
where

import qualified API.Types.UI.VehicleInfo as UIVI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo as DVI
import Domain.Types.VehicleRegistrationCertificate (VehicleRegistrationCertificate)
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common (generateGUID)
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (throwError)
import Storage.Queries.VehicleInfo (create, deleteAllByRcId, findAllByRcId)

getVehicleInfoList ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Types.Id.Id VehicleRegistrationCertificate ->
  Environment.Flow [DVI.VehicleInfo]
getVehicleInfoList _ = findAllByRcId

postVehicleInfoUpdate ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  UIVI.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postVehicleInfoUpdate _ req = do
  vrcId <- case req.newInfo of
    [] -> throwError $ InvalidRequest "VehicleInfo list is empty"
    (vi : _) -> pure vi.rcId
  deleteAllByRcId vrcId
  mapM_ updatedVehicleInfo req.newInfo
  pure Kernel.Types.APISuccess.Success
  where
    updatedVehicleInfo vehicleInfo = do
      newId <- generateGUID
      create $ vehicleInfo {DVI.id = newId}
