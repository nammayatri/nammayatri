module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
    getVehicleInfoListbyRcNo,
  )
where

import qualified API.Types.UI.VehicleInfo as UIVI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Storage.Queries.Vehicle as VQuery
import Storage.Queries.VehicleInfo (findAllByRcId)
import Storage.Queries.VehicleRegistrationCertificateExtra (findLastVehicleRCWrapper)
import Tools.Error
  ( DriverError (DriverWithoutVehicle),
    GenericError (InvalidRequest),
    VehicleError (VehicleDoesNotExist),
  )

getVehicleInfoList ::
  ( Kernel.Prelude.Maybe (ID.Id Domain.Types.Person.Person),
    ID.Id Domain.Types.Merchant.Merchant,
    ID.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Text ->
  Environment.Flow [UIVI.VehicleInfoAPIEntity]
getVehicleInfoList (mbDriverId, _, _) vrcNo = do
  linkedVehicle <-
    traverse VQuery.findById mbDriverId
      >>= fromMaybeM (DriverWithoutVehicle $ show mbDriverId) . join
  unless (linkedVehicle.registrationNo == vrcNo) . throwError $ InvalidRequest "Vehicle is not linked to this Driver"
  map convertVehicleInfoToVehicleInfoAPIEntity <$> getVehicleInfoListbyRcNo vrcNo
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} =
      UIVI.VehicleInfoAPIEntity
        { rcNo = vrcNo,
          ..
        }

getVehicleInfoListbyRcNo :: Kernel.Prelude.Text -> Environment.Flow [DVI.VehicleInfo]
getVehicleInfoListbyRcNo vrcNo = do
  rc <- findLastVehicleRCWrapper vrcNo >>= fromMaybeM (VehicleDoesNotExist vrcNo)
  findAllByRcId rc.id
