module Domain.Action.Dashboard.Management.VehicleInfo
  ( getVehicleInfoList,
    postVehicleInfoUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.VehicleInfo as Common
import qualified Domain.Action.UI.VehicleInfo as UIVI
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleInfo as DVI
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM)
import Storage.Queries.VehicleInfo (create, deleteAllByRcId)
import Storage.Queries.VehicleRegistrationCertificateExtra (findLastVehicleRCWrapper)
import Tools.Error (VehicleError (VehicleDoesNotExist))

getVehicleInfoList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Environment.Flow [Common.VehicleInfoAPIEntity]
getVehicleInfoList _merchantShortId _opCity vrcNo =
  map convertVehicleInfoToVehicleInfoAPIEntity <$> UIVI.getVehicleInfoListbyRcNo vrcNo
  where
    convertVehicleInfoToVehicleInfoAPIEntity DVI.VehicleInfo {..} =
      Common.VehicleInfoAPIEntity
        { rcNo = vrcNo,
          ..
        }

postVehicleInfoUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateVehicleInfoReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postVehicleInfoUpdate _merchantShortId _opCity req = do
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
