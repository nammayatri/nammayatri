{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Payout
  ( getPayoutPayoutOrder,
    getPayoutTransferNeedsReview,
    postPayoutTransferResolve,
  )
where

import qualified API.Types.RiderPlatform.Management.Payout as ApiPayout
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Time (minutesToTimeZone, utcToLocalTime)
import qualified Domain.Action.UI.Payout as UIPayout
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common (Minutes)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Domain.Types.Transfer as DT
import qualified Lib.Finance.Storage.Queries.Transfer as QTransfer
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Types.PayoutOrder as PayoutOrder
import qualified Lib.Payment.Payout.HoldRelease as TransferReview
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPayoutRequest
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getPayoutPayoutOrder ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Flow PayoutTypes.PayoutOrderResp
getPayoutPayoutOrder merchantShortId opCity payoutOrderId _mbRequestorId = do
  (merchant, _, timeZoneDiff) <- resolveMerchantOpCityAndTz merchantShortId opCity
  payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
  unless (payoutOrder.merchantId == merchant.id.getId) $
    throwError $ PayoutOrderNotFound payoutOrderId
  unless (payoutOrder.city == show opCity) $
    throwError $ PayoutOrderNotFound payoutOrderId
  refreshedOrder <- UIPayout.refreshPayoutOrderWithSettlement payoutOrder
  buildPayoutOrderResp timeZoneDiff refreshedOrder

resolveMerchantOpCityAndTz ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow (DM.Merchant, DMOC.MerchantOperatingCity, Minutes)
resolveMerchantOpCityAndTz merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  riderConfig <-
    getConfig
      (RiderConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId})
      Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist $ "merchantOperatingCityId:- " <> merchantOpCity.id.getId)
  pure (merchant, merchantOpCity, secondsToMinutes riderConfig.timeDiffFromUtc)

buildPayoutOrderResp ::
  Minutes ->
  PayoutOrder.PayoutOrder ->
  Flow PayoutTypes.PayoutOrderResp
buildPayoutOrderResp timeZoneDiff payoutOrder = do
  let timeZone = minutesToTimeZone timeZoneDiff.getMinutes
  person <- QPerson.findById (Id payoutOrder.customerId) >>= fromMaybeM (PersonNotFound payoutOrder.customerId)
  phoneNo <- decrypt payoutOrder.mobileNo
  pure
    PayoutTypes.PayoutOrderResp
      { payoutOrderId = payoutOrder.orderId,
        payoutOrderDbId = payoutOrder.id,
        driverId = payoutOrder.customerId,
        driverName = fromMaybe "" person.firstName,
        driverPhoneNo = phoneNo,
        amount = payoutOrder.amount.amount,
        transferAmount = payoutOrder.transferAmount,
        status = show payoutOrder.status,
        entityName = payoutOrder.entityName,
        entityIds = payoutOrder.entityIds,
        responseMessage = payoutOrder.responseMessage,
        responseCode = payoutOrder.responseCode,
        retriedOrderId = payoutOrder.retriedOrderId,
        vpa = payoutOrder.vpa,
        payoutTime = utcToLocalTime timeZone payoutOrder.createdAt,
        createdAt = payoutOrder.createdAt,
        updatedAt = payoutOrder.updatedAt
      }

-- | Transfers the finance kernel parked as NEEDS_REVIEW in this city, most recently touched first.
getPayoutTransferNeedsReview ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Flow ApiPayout.TransferReviewListRes
getPayoutTransferNeedsReview merchantShortId opCity mbLimit mbOffset _mbRequestorId = do
  (merchant, merchantOpCity, _) <- resolveMerchantOpCityAndTz merchantShortId opCity
  let limit = max 0 (min 100 (fromMaybe 20 mbLimit))
      offset = max 0 (fromMaybe 0 mbOffset)
  parked <- TransferReview.listParkedTransfers merchant.id.getId merchantOpCity.id.getId limit offset
  pure ApiPayout.TransferReviewListRes {items = map toReviewItem parked}
  where
    toReviewItem p =
      let t = p.transfer
       in ApiPayout.TransferReviewItem
            { transferId = t.id.getId,
              direction = show t.direction,
              railKey = show <$> t.railKey,
              referenceType = t.referenceType,
              referenceId = t.referenceId,
              railOrderId = t.railOrderId,
              railStatus = t.railStatus,
              amount = t.amount,
              feeAmount = t.feeAmount,
              currency = t.currency,
              ownerType = p.ownerType,
              ownerId = p.ownerId,
              lastError = t.lastError,
              attemptCount = t.attemptCount,
              holdEntryId = (.getId) <$> t.holdEntryId,
              entryIds = fromMaybe [] t.entryIds,
              coveredEntryIds = fromMaybe [] t.coveredEntryIds,
              canResolve = isResolvableTransfer t,
              createdAt = t.createdAt,
              updatedAt = t.updatedAt
            }

-- | Rider cashback and award payouts (metro, referral, backlog) can be settled by hand here; inbound
--   topups and refunds are listed for visibility but settle through their own flows.
isResolvableTransfer :: DT.Transfer -> Bool
isResolvableTransfer t = t.direction == DT.OUTBOUND && t.referenceType `elem` resolvableReferenceTypes

resolvableReferenceTypes :: [Text]
resolvableReferenceTypes =
  [ RidePaymentFinance.ridePaymentRefCashbackPayoutTransfer,
    RidePaymentFinance.ridePaymentRefMetroCashbackPayout,
    RidePaymentFinance.ridePaymentRefReferralAwardPayout
  ]

-- | Act on a parked rider payout: ask the partner again, or record ops' decision that it was paid or not.
--   A decision closes the payout order (who and why) and runs the same settlement as a partner status.
postPayoutTransferResolve ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  ApiPayout.TransferResolveReq ->
  Flow ApiPayout.TransferResolveRes
postPayoutTransferResolve merchantShortId opCity transferId mbRequestorId req = do
  (merchant, merchantOpCity, _) <- resolveMerchantOpCityAndTz merchantShortId opCity
  let reason = T.strip req.reason
      actor = fromMaybe "unknown" mbRequestorId
  when (req.action /= ApiPayout.RECHECK && T.null reason) $
    throwError (InvalidRequest "reason is required: say why this transfer is being resolved")
  -- One decision at a time per transfer: a double click must not settle twice.
  Redis.withWaitAndLockRedisWithTimeout ("PayoutTransferResolve:" <> transferId) 60 10 50000 $ do
    transfer <- TransferReview.loadParkedTransfer merchant.id.getId merchantOpCity.id.getId transferId
    unless (isResolvableTransfer transfer) $
      throwError (InvalidRequest "Only rider cashback and award payouts can be resolved here")
    order <- payoutOrderOf transfer
    logInfo $ "Parked transfer " <> transferId <> " " <> show req.action <> " by " <> actor <> ": " <> reason
    case req.action of
      ApiPayout.RECHECK -> void $ UIPayout.refreshPayoutOrderWithSettlement order
      ApiPayout.MARK_SUCCEEDED -> decide merchant merchantOpCity order TransferReview.ManualSucceeded actor reason
      ApiPayout.MARK_FAILED -> decide merchant merchantOpCity order TransferReview.ManualFailed actor reason
  after <- QTransfer.findById (Id transferId)
  let status = maybe "UNKNOWN" (show . (.status)) after
  pure
    ApiPayout.TransferResolveRes
      { transferId,
        status,
        message =
          if fmap (.status) after == Just DT.NEEDS_REVIEW
            then
              if req.action == ApiPayout.RECHECK
                then "Still awaiting review: the partner has not given a final answer yet. Decide by hand or re-check later."
                else "Still awaiting review: the order was updated but the ledger step did not finish. Check the logs and retry the same action."
            else "Transfer is now " <> status
      }
  where
    decide merchant merchantOpCity order decision actor reason = do
      payoutStatus <- TransferReview.closeOrderByDecision order decision actor reason
      UIPayout.runRiderPayoutSettlementManual merchant.id merchantOpCity.id payoutStatus order
    -- Awards are keyed on the payout order id; cashback on its payout request.
    payoutOrderOf transfer = do
      mbByOrderId <- QPayoutOrder.findByOrderId transfer.referenceId
      mbByRail <- maybe (pure Nothing) QPayoutOrder.findByOrderId transfer.railOrderId
      mbRequest <- QPayoutRequest.findById (Id transfer.referenceId)
      mbByRequest <- case mbRequest >>= (.payoutTransactionId) of
        Just orderDbId -> QPayoutOrder.findById (Id orderDbId)
        Nothing -> pure Nothing
      fromMaybeM (InvalidRequest "No payout order exists for this transfer, so there is nothing to settle against") (mbByOrderId <|> mbByRail <|> mbByRequest)
