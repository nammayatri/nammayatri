module ExternalBPP.Common.Init where

import qualified Beckn.ACL.FRFS.Init as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import BecknV2.OnDemand.Enums
import qualified Domain.Action.Beckn.FRFS.OnInit as DOnInit
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified ExternalBPP.EBIX.Flow as EBIXFlow
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import Tools.Error
import qualified Tools.Metrics as Metrics

init :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> DBooking.FRFSTicketBooking -> Flow ()
init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking = do
  case (bapConfig.vehicleCategory, bapConfig.provider) of
    (METRO, ONDC) -> do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      bknInitReq <- ACL.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} merchantOperatingCity.city
      logDebug $ "FRFS InitReq " <> encodeToText bknInitReq
      Metrics.startMetrics Metrics.INIT_FRFS merchant.name booking.searchId.getId merchantOperatingCity.id.getId
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id
    (BUS, EBIX) -> do
      onInitReq <- EBIXFlow.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking
      processOnInit onInitReq
    _ -> throwError $ InternalError ("Unsupported FRFS Flow vehicleCategory : " <> show bapConfig.vehicleCategory <> ", provider: " <> show bapConfig.provider)
  where
    processOnInit onInitReq = do
      (merchant', booking') <- DOnInit.validateRequest onInitReq
      DOnInit.onInit onInitReq merchant' booking'
