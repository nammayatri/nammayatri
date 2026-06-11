module Domain.Action.UI.VehicleInfo
  ( getVehicleInfoList,
  )
where

import qualified API.Types.UI.VehicleInfo as UIVI
import qualified Domain.Types.DocumentReminderHistory as DRH
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Storage.Queries.EntityInfo as QEI
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
getVehicleInfoList (mbDriverId, merchantId, _merchantOpCityId) vrcNo = do
  linkedVehicle <- traverse VQuery.findById mbDriverId >>= fromMaybeM (DriverWithoutVehicle $ show mbDriverId) . join
  unless (linkedVehicle.registrationNo == vrcNo) . throwError $ InvalidRequest "Vehicle is not linked to this Driver"
  rc <- findLastVehicleRCWrapper vrcNo >>= fromMaybeM (VehicleDoesNotExist vrcNo)
  vehicleInfo <- map convertVehicleInfoToVehicleInfoAPIEntity <$> findAllByRcId rc.id
  -- Inspection media now lives in MediaFile, referenced from entity_info (entityType RC); mediaUploaded = any media present
  entityInfos <- QEI.findAllByEntityIdAndType rc.id.getId DRH.RC merchantId
  let mediaUploaded = any (isJust . (.mediaFileId)) entityInfos
  pure $
    UIVI.VehicleExtraInformation
      { rcNo = vrcNo,
        vehicleInfo = vehicleInfo,
        mediaUploaded = mediaUploaded
      }
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} = UIVI.VehicleInfoAPIEntity {..}
