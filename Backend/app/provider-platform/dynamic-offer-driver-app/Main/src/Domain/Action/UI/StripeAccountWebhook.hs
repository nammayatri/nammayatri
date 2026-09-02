module Domain.Action.UI.StripeAccountWebhook
  ( stripeAccountWebhookHandler,
    stripeTestAccountWebhookHandler,
  )
where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import qualified Kernel.External.Payment.Interface as IPayment
import qualified Kernel.External.Payment.Interface.Stripe as IStripe
import qualified Kernel.External.Payment.Stripe.AccountWebhook as StripeAccountWh
import qualified Kernel.External.Payment.Stripe.Types.AccountWebhook as StripeAccountTy
import qualified Kernel.External.Payment.Stripe.Types.Common as PaymentStripe
import qualified Kernel.External.Payment.Types as TPayment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Servant (RawByteString (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA

stripeAccountWebhookHandler,
  stripeTestAccountWebhookHandler ::
    ShortId DM.Merchant ->
    Maybe Context.City ->
    Maybe Text ->
    RawByteString ->
    Flow AckResponse
stripeAccountWebhookHandler = stripeAccountWebhookHandler' DMPM.LIVE
stripeTestAccountWebhookHandler = stripeAccountWebhookHandler' DMPM.TEST

stripeAccountWebhookHandler' ::
  DMPM.PaymentMode ->
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Text ->
  RawByteString ->
  Flow AckResponse
stripeAccountWebhookHandler' paymentMode merchantShortId mbOpCity mbSigHeader rawBytes = do
  let paymentService = case paymentMode of
        DMPM.LIVE -> TPayment.Stripe
        DMPM.TEST -> TPayment.StripeTest
  (paymentServiceConfig, merchantOperatingCityId, _merchantId) <- fetchPaymentServiceConfig merchantShortId mbOpCity paymentService
  let checkDuplicatedEvent eventId =
        isDuplicateStripeWebhookEvent
          (stripeAccountWebhookEventDedupKey paymentMode eventId)
          stripeWebhookEventDedupTtl
  StripeAccountWh.accountServiceEventWebhook
    paymentServiceConfig
    checkDuplicatedEvent
    (stripeAccountWebhookAction paymentMode merchantOperatingCityId)
    mbSigHeader
    rawBytes

stripeWebhookEventDedupTtl :: Redis.ExpirationTime
stripeWebhookEventDedupTtl = 3 * 24 * 60 * 60

stripeAccountWebhookEventDedupKey :: DMPM.PaymentMode -> Id PaymentStripe.Event -> Text
stripeAccountWebhookEventDedupKey paymentMode eventId =
  "Stripe:Account:" <> show paymentMode <> ":Webhook:Event-" <> eventId.getId

isDuplicateStripeWebhookEvent :: (Redis.HedisFlow m env, TryException m) => Text -> Redis.ExpirationTime -> m Bool
isDuplicateStripeWebhookEvent key ttl =
  not <$> Redis.setNxExpire key ttl True

stripeAccountWebhookAction ::
  DMPM.PaymentMode ->
  Id DMOC.MerchantOperatingCity ->
  StripeAccountTy.AccountStripeWebhookReq ->
  Text ->
  Flow AckResponse
stripeAccountWebhookAction paymentMode _mocId req _respDump = do
  let livemodeMatches = req.livemode == (paymentMode == DMPM.LIVE)
  if not livemodeMatches
    then do
      logInfo $ "livemode mismatch for Stripe account webhook: paymentMode=" <> show paymentMode <> " req.livemode=" <> show req.livemode
      pure Ack
    else case req._data._object of
      StripeAccountTy.ObjectAccount acct -> applyAccountUpdate paymentMode req acct
      StripeAccountTy.AccountStripeWebhookCustomObject objType _val -> do
        logInfo $ "Unhandled Stripe account webhook object type: " <> objType
        pure Ack

applyAccountUpdate ::
  DMPM.PaymentMode ->
  StripeAccountTy.AccountStripeWebhookReq ->
  StripeAccountTy.AccountObject ->
  Flow AckResponse
applyAccountUpdate paymentMode req acct = do
  mbRow <- QDBA.findByAccountId acct.id
  case mbRow of
    Nothing -> do
      logInfo $ "Stripe account webhook for unknown accountId=" <> show acct.id
      pure Ack
    Just row | fromMaybe DMPM.LIVE row.paymentMode /= paymentMode -> do
      logInfo $
        "Stripe account webhook mode mismatch; ignoring. accountId=" <> show acct.id
          <> " endpointMode="
          <> show paymentMode
          <> " rowMode="
          <> show row.paymentMode
      pure Ack
    Just row -> do
      let eventTime = posixSecondsToUTCTime req.created
      if maybe False (>= eventTime) row.lastSyncedAt
        then do
          logInfo $ "Stripe account webhook stale event, ignoring. accountId=" <> show acct.id
          pure Ack
        else do
          let requirements = IStripe.toRequirementsInfo <$> acct.requirements
              futureRequirements = IStripe.toRequirementsInfo <$> acct.future_requirements
          QDBA.updateFromWebhook
            acct.charges_enabled
            acct.payouts_enabled
            acct.details_submitted
            requirements
            futureRequirements
            row.driverId
          pure Ack

fetchPaymentServiceConfig ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  TPayment.PaymentService ->
  Flow (IPayment.PaymentServiceConfig, Id DMOC.MerchantOperatingCity, Id DM.Merchant)
fetchPaymentServiceConfig merchantShortId mbOpCity paymentService = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOperatingCityId <- CQMOC.getMerchantOpCityId Nothing merchant mbOpCity
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = Nothing, serviceName = Just (DEMSC.PaymentService paymentService)}) Nothing
      >>= fromMaybeM (MerchantServiceConfigNotFound merchant.id.getId "Payments" (show paymentService))
  case merchantServiceConfig.serviceConfig of
    DMSC.PaymentServiceConfig psc -> pure (psc, merchantOperatingCityId, merchant.id)
    _ -> throwError $ InternalError "Unknown Service Config"
