module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
  )
where

import qualified API.Types.UI.VehicleInfo as UIVI
import Domain.Types.Common
import Domain.Types.MediaFileDocument
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Storage.Queries.MediaFileDocument as QMFD
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
  Environment.Flow UIVI.VehicleExtraInformation
getVehicleInfoList (mbDriverId, _, merchantOpCityId) vrcNo = do
  linkedVehicle <- traverse VQuery.findById mbDriverId >>= fromMaybeM (DriverWithoutVehicle $ show mbDriverId) . join
  unless (linkedVehicle.registrationNo == vrcNo) . throwError $ InvalidRequest "Vehicle is not linked to this Driver"
  rc <- findLastVehicleRCWrapper vrcNo >>= fromMaybeM (VehicleDoesNotExist vrcNo)
  vehicleInfo <- map convertVehicleInfoToVehicleInfoAPIEntity <$> findAllByRcId rc.id
  mediaFileDocument <- QMFD.findOneByCityRcTypeAndStatus merchantOpCityId rc.id VehicleVideo [CONFIRMED, COMPLETED]
  pure $
    UIVI.VehicleExtraInformation
      { rcNo = vrcNo,
        vehicleInfo = vehicleInfo,
        mediaUploaded = isJust mediaFileDocument
      }
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} = UIVI.VehicleInfoAPIEntity {..}
