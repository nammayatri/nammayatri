{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Internal.Payout
  ( juspayPayoutWebhookHandler,
    payoutProcessingLockKey,
    castOrderStatus,
  )
where

import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.VehicleCategory as DV
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
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import Servant (BasicAuthData)
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CQPayoutCfg
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey = UIPayout.payoutProcessingLockKey

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
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchanOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName'}) Nothing
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
      UIPayout.runRiderPayoutSettlement merchantId merchanOperatingCityId payoutStatus payoutOrder
    IPayout.BadStatusResp -> pure ()
  pure Ack

castOrderStatus :: Payout.PayoutOrderStatus -> DPS.PayoutStatus
castOrderStatus = UIPayout.castOrderStatus
