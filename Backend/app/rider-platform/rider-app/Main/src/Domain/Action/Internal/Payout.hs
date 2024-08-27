module Domain.Action.Internal.Payout
  ( juspayPayoutWebhookHandler,
    payoutProcessingLockKey,
  )
where

import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import Tools.Error
import qualified Tools.Payout as Payout

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey bookingId = "Payout:Processing:bookingId" <> bookingId

-- webhook ----------------------------------------------------------

juspayPayoutWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayPayoutWebhookHandler merchantShortId mbOpCity authData value = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId merchant mbOpCity
  let merchantId = merchant.id
      serviceName' = DEMSC.PayoutService TPayout.Juspay
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchanOperatingCityId serviceName'
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show TPayout.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  logDebug $ "Webhook Payout Resp: " <> show osr
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
      unless (payoutOrder.status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]) do
        case payoutOrder.entityName of
          Just DPayment.METRO_BOOKING_CASHBACK -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \bookingId -> do
              when (payoutStatus `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]) do
                QFTB.updatePayoutStatusById (Just $ castPayoutOrderStatus payoutStatus) (Id bookingId)
              fork "Update Payout Status and Transactions for MetroBooking" $ do
                callPayoutService (Id bookingId) payoutOrderId
          _ -> throwError $ InternalError $ "Unsupported Payout Entity: " <> show payoutOrder.entityName
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    callPayoutService bookingId payoutOrderId = do
      booking <- B.runInReplica $ QFTB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrderId, mbExpand = Nothing}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus booking.merchantId booking.merchantOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast booking.merchantId) (cast booking.riderId) createPayoutOrderStatusReq createPayoutOrderStatusCall

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DFTB.CashbackStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DFTB.SUCCESSFUL
    Payout.FULFILLMENTS_SUCCESSFUL -> DFTB.SUCCESSFUL
    Payout.ERROR -> DFTB.CASHBACK_FAILED
    Payout.FAILURE -> DFTB.CASHBACK_FAILED
    Payout.FULFILLMENTS_FAILURE -> DFTB.CASHBACK_FAILED
    Payout.CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DFTB.MANUAL_VERIFICATION
    _ -> DFTB.PROCESSING
