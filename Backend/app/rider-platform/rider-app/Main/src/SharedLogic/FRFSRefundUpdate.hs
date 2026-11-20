module SharedLogic.FRFSRefundUpdate (sendRefundUpdateToBpp) where

import qualified Beckn.ACL.FRFS.Update as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity as Fields
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import Tools.Error

-- | After the rider's refund for a cancelled ONDC booking succeeds at the payment gateway,
-- tell the BPP via `update` (update_target = order.payments) so it can close the refund on its side.
sendRefundUpdateToBpp ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasRequestId r
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  Id DOrder.PaymentOrder ->
  Maybe Text ->
  HighPrecMoney ->
  m ()
sendRefundUpdateToBpp booking paymentOrderId mbRefundArn refundedAmount =
  fork ("FRFS ONDC refund update: " <> booking.id.getId) $ do
    integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
    case integratedBPPConfig.providerConfig of
      ONDC _ -> do
        mbRrn <- maybe (FRFSUtils.getChargedPaymentRrn paymentOrderId) (pure . Just) mbRefundArn
        case mbRrn of
          Nothing -> logError $ "FRFS refund update skipped: no refund ARN and no RRN on CHARGED payment_transaction for paymentOrderId " <> paymentOrderId.getId <> ", booking " <> booking.id.getId
          Just rrn -> do
            merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
            let vehicleCategory = Utils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType
            bapConfig <-
              getOneConfig (BecknConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId, merchantId = booking.merchantId.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just vehicleCategory, becknProtocol = Nothing}) (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking.merchantOperatingCityId booking.merchantId (show Spec.FRFS) vehicleCategory))
                >>= fromMaybeM (InternalError "Beckn Config not found")
            providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
            messageId <- generateGUID
            let city = SIBC.resolveOndcCity integratedBPPConfig merchantOperatingCity.city
            updateReq <- ACL.buildRefundUpdateReq messageId booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} city rrn refundedAmount booking.totalPrice.currency
            logInfo $ "FRFS refund UpdateReq " <> encodeToText updateReq
            void $ CallFRFSBPP.update providerUrl updateReq booking.merchantId
      _ -> pure ()
