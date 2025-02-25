module Tools.Dashcam where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Dashcam.Domain.Cautio.Types as Dashcam
import qualified Lib.Dashcam.Domain.Interface as DashcamInter
import qualified Lib.Dashcam.Domain.Types as Dashcam
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

cautioInstallationStatus :: ServiceFlow m r => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> Dashcam.InstallationStatusReq -> m [Dashcam.InstallationRespEntity]
cautioInstallationStatus = runWithServiceConfigAndName $ DashcamInter.cautioInstallationStatus

runWithServiceConfigAndName ::
  ServiceFlow m r =>
  (Dashcam.CautioConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  req ->
  m resp
runWithServiceConfigAndName func merchantId merchantOperatingCity serviceName req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCity
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Dashcam" (show Dashcam.Cautio))
  case merchantServiceConfig.serviceConfig of
    DMSC.DashCamServiceConfig vsc -> do
      case vsc of
        DashcamInter.CautioConfig sc -> func sc req
    _ -> throwError $ InternalError "Unknown Service Config"
