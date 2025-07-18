module Domain.Action.Dashboard.Management.VehicleInfo
  ( getVehicleInfoList,
    postVehicleInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo as Common
import qualified Data.List as DL
import Domain.Types.Common
import Domain.Types.MediaFileDocument
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.MediaFileDocument as QMFD
import Storage.Queries.VehicleInfo (create, deleteAllByRcId, findAllByRcId)
import Storage.Queries.VehicleRegistrationCertificateExtra (findLastVehicleRCWrapper)
import Tools.Error

getVehicleInfoList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Environment.Flow Common.VehicleExtraInformation
getVehicleInfoList merchantShortId opCity vrcNo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  rc <- findLastVehicleRCWrapper vrcNo >>= fromMaybeM (VehicleDoesNotExist vrcNo)
  vehicleInfo <- map convertVehicleInfoToVehicleInfoAPIEntity <$> findAllByRcId rc.id
  mediaFileDocument <- QMFD.findOneByCityRcTypeAndStatus merchantOpCityId rc.id VehicleVideo [CONFIRMED, COMPLETED]
  pure $ Common.VehicleExtraInformation {rcNo = vrcNo, vehicleInfo = vehicleInfo, mediaUploaded = isJust mediaFileDocument}
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} =
      Common.VehicleInfoAPIEntity
        { ..
        }

postVehicleInfoUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postVehicleInfoUpdate _merchantShortId _opCity req = do
  let questionIds = req.newInfo <&> (.questionId)
  unless (length (DL.nub questionIds) == length questionIds) $
    throwError (InvalidRequest "questionId should be unique")
  rc <- findLastVehicleRCWrapper req.rcNo >>= fromMaybeM (VehicleDoesNotExist req.rcNo)
  unless (null req.newInfo) $ do
    deleteAllByRcId rc.id
    forM_ req.newInfo $ updatedVehicleInfo rc.id
  pure Kernel.Types.APISuccess.Success
  where
    updatedVehicleInfo rcId Common.VehicleInfoPostData {..} =
      create $
        DVI.VehicleInfo
          { rcId = rcId,
            ..
          }
