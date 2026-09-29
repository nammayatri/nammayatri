{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | What happens to one payout_order once its fate is known: settled, failed, or excluded before it
-- was ever sent. Every ledger release and every beneficiary notification for a bulk payout is here.
module SharedLogic.Payout.Bulk.Order
  ( BulkOutcomeCtx,
    loadBulkOutcomeCtx,
    settleClaimedOrder,
    failClaimedOrder,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (LedgerEntryMetadata (..))
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Ledger.PayoutSettlement (PayoutOutcome (..))
import Lib.Finance.Ledger.Service (markEntriesAsClaimed)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Request (clearPayoutLedgerEntryIds, getPayoutLedgerEntryIds, updateStatusWithHistoryById)
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import SharedLogic.Finance.Wallet (buildDriverChargeCtx, counterpartyFromRole, releaseWalletEntriesReservation, settleWalletEntries, settleWalletPayoutLedger)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify

-- | What applying outcomes to a batch's orders needs beyond the orders themselves. Every order in
--   a batch shares one city, so all of it is read once per batch rather than once per item: the
--   per-item reads used to be three lookups and a config read for every row HDFC reported.
data BulkOutcomeCtx = BulkOutcomeCtx
  { requestsById :: Map.Map Text PR.PayoutRequest,
    personsById :: Map.Map Text DP.Person,
    mbTransporterConfig :: Maybe DTConf.TransporterConfig
  }

loadBulkOutcomeCtx ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, PaymentBeamFlow.BeamFlow m r) =>
  Text -> -- the batch's merchantOperatingCityId
  [DPayoutOrder.PayoutOrder] ->
  m BulkOutcomeCtx
loadBulkOutcomeCtx merchantOpCityId orders = do
  requests <- QPayoutRequestExtra.findByIds (mapMaybe (listToMaybe <=< (.entityIds)) orders)
  persons <- QPerson.findAllByPersonIds (nub (map (.customerId) orders <> map (.beneficiaryId) requests))
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId}) Nothing
  pure
    BulkOutcomeCtx
      { requestsById = Map.fromList [(r.id.getId, r) | r <- requests],
        personsById = Map.fromList [(p.id.getId, p) | p <- persons],
        mbTransporterConfig
      }

requestOf :: BulkOutcomeCtx -> DPayoutOrder.PayoutOrder -> Maybe PR.PayoutRequest
requestOf ctx order = (`Map.lookup` ctx.requestsById) =<< listToMaybe =<< order.entityIds

-- | Fail one claimed order with the partner's own code and text, and release its reservation.
--
--   No category of ours is recorded. Every rejection the adapter can produce -- a gateway refusal
--   before the payment engine, an item the engine rejected, a post-debit return -- used to collapse
--   onto one value, which read the same for all three and was actively wrong for the first. What
--   distinguishes them is already stored: the batch's own status says whether the file was ever sent,
--   and @transferStatus@ says whether money had moved (TRANSFER_FAILED for a return, absent for a
--   validation rejection).
failClaimedOrder ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  BulkOutcomeCtx ->
  DPayoutOrder.PayoutOrder ->
  -- | The coded axis that refused it, as the partner sent it: @codstatus@, @rbistatus@, or a gateway
  --   code. Its own column, so "every order that hit this code" needs no prose matching.
  Maybe Text ->
  -- | The partner's reason, or Nothing when they sent none. Not defaulted to prose of ours: this
  --   text reaches the driver's notification and recon's failure reason, where something we made up
  --   would be indistinguishable from something the bank said.
  Maybe Text ->
  -- | Settlement (rbistatus) mirror to store as transferStatus: TRANSFER_FAILED for a TXREJE
  --   return, Nothing for a validation/submission rejection that never reached RBI settlement.
  Maybe Payout.TransferStatus ->
  m ()
failClaimedOrder ctx order mbCode detail mbSettleStatus = do
  -- The partner's own words go on the order itself, in the same write as the status. 'detail' is
  -- whichever reason field applied -- the rejection text for a validation refusal, the return text
  -- for a post-debit return -- and the two never arrive together, so one column holds it.
  QPayoutOrder.updateBulkStatusAndRespInfo Payout.FAILURE mbSettleStatus mbCode detail order.orderId
  whenJust (requestOf ctx order) $ \request -> do
    -- The money is ours again -- either it never left, or the beneficiary bank returned it -- so
    -- the hold is reversed and the earnings go back to payable for the next sweep.
    applyBulkPayoutOutcome ctx request (PayoutFailed (fromMaybe "payout failed" detail))
    updateStatusWithHistoryById PR.FAILED detail request
    -- failureReason is what the API returns and what recon reads; without this the partner's
    -- text is only recoverable by joining finance_state_transition.
    QPR.updateStatusWithReasonById PR.FAILED detail request.id
  -- Forked: this runs inside the sequential per-item outcome loop, so a blocking FCM call must not
  -- hold up the rest of the batch.
  fork ("BulkPayoutNotify:" <> order.orderId) $ notifyBulkPayoutOutcome (Map.lookup order.customerId ctx.personsById) order detail

-- | Settle a claimed order HDFC CBX reports as processed: mark it paid and release the ledger
--   entries into PAID_OUT.
settleClaimedOrder ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  BulkOutcomeCtx ->
  DPayoutOrder.PayoutOrder ->
  Text -> -- settlementRef (UTR / FT number / RRN, per refType)
  Payout.SettlementRefType ->
  -- | Settlement (rbistatus) mirror to store as transferStatus: TRANSFERRED on a confirmed credit,
  --   DEEMED_SETTLED when the clearing system deems it made, Nothing for an intra-bank payout (no RBI
  --   settlement layer -- the debit is the credit).
  Maybe Payout.TransferStatus ->
  -- | The partner's coded axes and their word for how it settled, refreshed here so a settled row does
  --   not keep whatever the last in-flight pass recorded.
  Maybe Text ->
  Maybe Text ->
  m ()
settleClaimedOrder ctx order settlementRef refType mbSettleStatus mbCode mbNote = do
  -- The settlement instrument lives on payout_order only. payout_request reaches it the way every
  -- other payout flow does, through payoutTransactionId (the order id).
  QPayoutOrder.updateBulkSettled Payout.SUCCESS mbSettleStatus (Just settlementRef) (Just refType) mbCode mbNote order.orderId
  whenJust (requestOf ctx order) $ \request -> do
    applyBulkPayoutOutcome ctx request PayoutSucceeded
    updateStatusWithHistoryById PR.CREDITED Nothing request
  -- Forked for the same reason as failClaimedOrder's notify call -- see comment there.
  fork ("BulkPayoutNotify:" <> order.orderId) $ notifyBulkPayoutOutcome (Map.lookup order.customerId ctx.personsById) order Nothing

-- | Apply a terminal outcome to the beneficiary's wallet ledger, the same way the Juspay webhook
--   does (see Domain.Action.UI.Payout).
--
--   This is the double-pay fix. The old model reserved the *entries* at claim but derived the
--   payable amount from the wallet balance, which the reservation does not reduce -- so a second
--   sweep could see the same money and pay it again. 'initiateWalletPayout' now holds the amount
--   (OwnerLiability -> OwnerPayoutLiability) instead, which the balance does exclude, and this
--   settles or reverses that hold.
--
--   Safe to run repeatedly: 'settlePayoutLedger' looks every leg up before posting it, which is
--   exactly what the status-check job needs -- it re-resolves a batch on every inquiry round, and
--   HDFC repeat the same rows each time.
--
--   The entry-level settle/release that follows is kept for payouts claimed before the hold
--   existed, which did reserve entries as PROCESSING; for a held payout the id list is empty and
--   both calls are no-ops.
applyBulkPayoutOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Finance.HasActorInfo m r, BeamFlow m r, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  BulkOutcomeCtx ->
  PR.PayoutRequest ->
  PayoutOutcome ->
  m ()
applyBulkPayoutOutcome ctx request outcome = do
  let amount = fromMaybe 0 request.amount
  case (Map.lookup request.beneficiaryId ctx.personsById, ctx.mbTransporterConfig) of
    (Just person, Just transporterConfig) -> do
      let counterparty = counterpartyFromRole person.role
          walletCtx = buildDriverChargeCtx counterparty request.beneficiaryId request.merchantId request.merchantOperatingCityId transporterConfig.currency request.id.getId (fromMaybe False transporterConfig.driverWalletConfig.enableWalletGatedTierCheck)
          holdMetadata =
            LedgerEntryMetadata
              { driverPayable = Just (negate amount),
                payoutOrderId = Nothing,
                reason = Nothing,
                subscriptionAllocations = Nothing,
                d2cReferralEarnings = Nothing,
                d2dReferralEarnings = Nothing,
                dailyStatsId = Nothing
              }
      res <- settleWalletPayoutLedger walletCtx amount (Just holdMetadata) outcome
      case res of
        Left err -> logError $ "Bulk payout ledger settlement failed for payoutRequest " <> request.id.getId <> ": " <> show err
        Right ledgerIds -> do
          entryIds <- map Id <$> getPayoutLedgerEntryIds request
          case outcome of
            PayoutSucceeded -> do
              markEntriesAsClaimed ledgerIds
              unless (null entryIds) $ settleWalletEntries entryIds request.id.getId
            PayoutFailed _ -> unless (null entryIds) $ releaseWalletEntriesReservation entryIds
          clearPayoutLedgerEntryIds request.id.getId
    _ ->
      -- Deliberately loud and non-fatal: the order's own status is already written, and a missing
      -- person or city config is a data problem for a human, not a reason to abandon the batch.
      logError $ "Bulk payout ledger settlement skipped for payoutRequest " <> request.id.getId <> ": person or transporter config not found"

-- | Notify the beneficiary of a terminal bulk-payout outcome. On failure, the message names the
--   actual reason HDFC CBX gave instead of a generic "failed".
--   Drivers only for now -- FCM is the only notification channel wired up; fleet owners have no
--   channel decided yet, so they're skipped rather than silently sent to a driver-shaped push.
-- | @mbFailureDetail@ is the partner's own text, passed through rather than mapped to wording of
--   ours: the mapping used to be a substring match over the partner's prose, which decided what a
--   driver was told on the strength of a phrase the bank can reword at any time.
notifyBulkPayoutOutcome ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  Maybe DP.Person -> -- the order's customer, from the batch's pre-loaded persons
  DPayoutOrder.PayoutOrder ->
  Maybe Text -> -- Nothing on success
  m ()
notifyBulkPayoutOutcome mbPerson order mbFailureDetail =
  whenJust mbPerson $ \person -> when (person.role `notElem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $ do
    let amount = order.amount.amount
        (notificationTitle, notificationMessage, notificationType) = case mbFailureDetail of
          Nothing -> ("Payout Complete", "Your payout of Rs." <> show amount <> " has been successfully settled to your bank account.", FCM.PAYOUT_COMPLETED)
          Just detail ->
            ( "Payout Failed",
              "Your payout of Rs." <> show amount <> " has failed: " <> detail <> ". Please retry or contact support.",
              FCM.PAYOUT_FAILED
            )
    Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing notificationType notificationTitle notificationMessage person person.deviceToken
