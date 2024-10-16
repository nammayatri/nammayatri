module ExternalBPP.Common.Status where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import BecknV2.OnDemand.Enums
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified ExternalBPP.EBIX.Flow as EBIXFlow
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import Storage.CachedQueries.IntegratedBPPConfig as QIBC
import Tools.Error

status :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> DBooking.FRFSTicketBooking -> Flow ()
status merchantId merchantOperatingCity bapConfig booking = do
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= return . fmap (.providerConfig)
  case (bapConfig.vehicleCategory, integratedBPPConfig) of
    (METRO, _) -> do
      void $ CallFRFSBPP.callBPPStatus booking bapConfig merchantOperatingCity.city merchantId
    (BUS, Just (EBIX config)) -> do
      onStatusReq <- EBIXFlow.status merchantId merchantOperatingCity config bapConfig booking
      processOnStatus onStatusReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory)
  where
    processOnStatus onStatusReq = do
      (merchant', booking') <- DOnStatus.validateRequest onStatusReq
      DOnStatus.onStatus merchant' booking' onStatusReq
