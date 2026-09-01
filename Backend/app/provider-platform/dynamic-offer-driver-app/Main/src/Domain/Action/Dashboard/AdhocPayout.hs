{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.AdhocPayout
  ( lookupPayoutEligibility,
    initiateAdhocPayouts,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import Data.List (groupBy, partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Domain.Action.UI.DriverWallet as DriverWallet
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan (ServiceNames (PREPAID_SUBSCRIPTION))
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutRun as DDPR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Environment
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout
  ( claimBulkPayoutSlot,
    createAdhocPayoutRun,
    failClaimedOrder,
    scheduleBulkPollJob,
    submitBulkBatch,
  )
import SharedLogic.Finance.Wallet
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.PayoutRun as QPayoutRun
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Payout as TP

-- | A person resolved and validated for an adhoc payout attempt -- everything the claim/submit
--   step needs, computed once per person.
data ResolvedPerson = ResolvedPerson
  { person :: DP.Person,
    merchantOpCity :: DMOC.MerchantOperatingCity,
    transporterConfig :: DTConf.TransporterConfig,
    config :: DSPC.ScheduledPayoutConfig,
    payoutServiceFlow :: Payout.PayoutServiceFlow,
    payoutServiceName :: DEMSC.ServiceName,
    isManuallyAdded :: Bool
  }

-- | True if this person's payout VPA/bank status is MANUALLY_ADDED -- same skip condition the
--   scheduled sweep applies, checked per-person here since there's no batch eligibility query.
resolveIsManuallyAdded :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Person -> m Bool
resolveIsManuallyAdded person
  | person.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] = do
    mbFleetInfo <- QFOI.findByPrimaryKey person.id
    pure $ (mbFleetInfo >>= (.payoutVpaStatus)) == Just DFOI.MANUALLY_ADDED
  | otherwise = do
    mbDriverInfo <- QDI.findByPrimaryKey person.id
    pure $ (mbDriverInfo >>= (.payoutVpaStatus)) == Just DI.MANUALLY_ADDED

-- | Resolve + validate one person for an adhoc payout: must exist, must belong to the
--   dashboard-authenticated merchant, and the city must have a ScheduledPayoutConfig row seeded
--   (source of truth for minimumPayoutAmount/itemsPerBatch/defaultPayoutRail -- isEnabled is
--   deliberately ignored, since adhoc exists to bypass the scheduled-sweep gate).
resolvePerson :: DM.Merchant -> Id.Id DP.Person -> Environment.Flow ResolvedPerson
resolvePerson merchant personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.merchantId == merchant.id) $ throwError (InvalidRequest "Person does not belong to this merchant")
  merchantOpCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  config <-
    getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, isEnabled = Nothing, payoutCategory = Just DPayment.DRIVER_WALLET_TRANSACTION}) Nothing
      >>= fromMaybeM (InvalidRequest "No ScheduledPayoutConfig seeded for this city; seed one via scheduledPayoutConfig/upsert (isEnabled can stay false)")
  (payoutServiceFlow, payoutServiceName) <- TP.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TP.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCity.id
  isManuallyAdded <- resolveIsManuallyAdded person
  pure ResolvedPerson {person, merchantOpCity, transporterConfig, config, payoutServiceFlow, payoutServiceName, isManuallyAdded}

tryResolvePerson :: DM.Merchant -> Id.Id DP.Person -> Environment.Flow (Either SomeException ResolvedPerson)
tryResolvePerson merchant personId = try (resolvePerson merchant personId)

-- | Compute a person's current wallet balance / payoutable balance for display, before an admin
--   decides to include them in an adhoc initiate call.
lookupPayoutEligibility ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id DP.Person ->
  Environment.Flow ApiPayout.AdhocPayoutEligibilityResp
lookupPayoutEligibility merchantShortId _opCity personId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  rp <- resolvePerson merchant personId
  let counterparty = counterpartyFromRole rp.person.role
  now <- getCurrentTime
  mbAccount <- getWalletAccountByOwner counterparty personId.getId
  walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty personId.getId
  let timeDiff = secondsToNominalDiffTime rp.transporterConfig.timeDiffFromUtc
      cutoff = payoutCutoffTimeUTC timeDiff rp.transporterConfig.driverWalletConfig.payoutCutOffDays now
  (nonRedeemable, _redeemableIds, _merchantTransferAmt) <- case (.id) <$> mbAccount of
    Nothing -> pure (0, [], 0)
    Just accountId -> getPayoutEligibilityData accountId cutoff now
  let payoutableBalance = walletBalance - nonRedeemable
      isEligible = payoutableBalance >= rp.config.minimumPayoutAmount
  (_payoutServiceFlow, _payoutServiceName, mbBankAccount) <-
    TP.getCreatePayoutServiceFlow (TP.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService rp.person.clientSdkVersion rp.merchantOpCity.id personId
  pure
    ApiPayout.AdhocPayoutEligibilityResp
      { personId = personId.getId,
        personName = Just rp.person.firstName,
        role = show rp.person.role,
        merchantOperatingCityId = rp.merchantOpCity.id.getId,
        walletBalance = walletBalance,
        nonRedeemableAmount = nonRedeemable,
        payoutableBalance = payoutableBalance,
        minimumPayoutAmount = rp.config.minimumPayoutAmount,
        isEligible = isEligible,
        payoutServiceFlow = show rp.payoutServiceFlow,
        bankAccountStatus = maybe "MISSING" (const "PRESENT") mbBankAccount
      }

-- | Push a payout right now for each of the given person ids. One bad/ineligible id never fails
--   the whole request -- each gets its own INITIATED/SKIPPED/FAILED result. BulkFlow (HDFC CBX)
--   people are grouped per (city, flow) into a single adhoc payout_run + one submitBulkBatch call
--   per group; Juspay/Stripe people are handled individually and synchronously (their contract
--   needs a result before this returns, unlike the scheduled sweep's fire-and-forget fork).
initiateAdhocPayouts ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  [Id.Id DP.Person] ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
initiateAdhocPayouts merchantShortId _opCity personIds = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  resolved <- forM personIds $ \pid -> (,) pid <$> tryResolvePerson merchant pid
  let failures = [(pid, e) | (pid, Left e) <- resolved]
      successes = [rp | (_, Right rp) <- resolved]
      failureResults =
        [ ApiPayout.AdhocPayoutResultItem {personId = pid.getId, status = ApiPayout.FAILED, reason = Just (show e), payoutOrderId = Nothing}
          | (pid, e) <- failures
        ]
      groupKey :: ResolvedPerson -> (Text, String)
      groupKey rp = (rp.merchantOpCity.id.getId, show rp.payoutServiceFlow)
      groups = groupBy (\a b -> groupKey a == groupKey b) (sortOn groupKey successes)
      (bulkGroups, nonBulkGroups) = partition (\g -> (head g).payoutServiceFlow == Payout.BulkFlow) groups
  bulkResults <- concat <$> forM bulkGroups (processBulkGroup merchant.id)
  nonBulkResults <- concat <$> forM nonBulkGroups processNonBulkGroup
  pure $ ApiPayout.AdhocPayoutInitiateResp (failureResults <> bulkResults <> nonBulkResults)

-- | Claim + submit one (city, BulkFlow) group of people as a single HDFC CBX bulk submission
--   under one fresh adhoc payout_run, then spin up the poll job that drives it to resolution.
--   Guarded end-to-end: a failure creating the run fails the whole group (nothing was claimed
--   yet, so nothing to release); a failure in submitBulkBatch/scheduling the poll job -- after
--   claims already reserved ledger entries -- releases those specific claimed orders via
--   failClaimedOrder instead of leaving them stuck with no payout_batch/poll job to ever revisit
--   them. Either way this function itself never throws, so one group's failure can't take down
--   the rest of the request.
processBulkGroup :: Id.Id DM.Merchant -> [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processBulkGroup merchantId group = do
  let rp0 = head group
      merchantOpCityId = rp0.merchantOpCity.id
      config = rp0.config
      payoutServiceName = rp0.payoutServiceName
      transporterConfig = rp0.transporterConfig
  runResult <- try $ createAdhocPayoutRun merchantId merchantOpCityId transporterConfig.currency Payout.BulkFlow
  case runResult of
    Left (e :: SomeException) ->
      pure [failedItem rp ("Could not start adhoc payout run: " <> show e) | rp <- group]
    Right run -> do
      claimResults <- forM group $ \rp -> do
        mbOrder <- claimBulkPayoutSlot config PR.ADHOC transporterConfig merchantId merchantOpCityId run.id.getId rp.person.id rp.isManuallyAdded
        pure (rp, mbOrder)
      let claimedOrders = mapMaybe snd claimResults
      if null claimedOrders
        then pure [skippedItem rp | (rp, _) <- claimResults]
        else do
          -- buildBulkItem (inside submitBulkBatch) already releases/fails any individually
          -- excluded order (e.g. no bank account on file) at the source -- excludedOrders here is
          -- only needed to report those people accurately, not to release anything ourselves.
          submitResult <-
            try $ do
              excludedOrders <- submitBulkBatch config payoutServiceName merchantId merchantOpCityId run.id.getId transporterConfig.currency DPayoutBatch.ADHOC claimedOrders
              batches <- QPayoutBatch.findAllByRunId (Just run.id.getId)
              pure (excludedOrders, batches)
          case submitResult of
            Left (e :: SomeException) -> do
              -- Whole-group failure before/without buildBulkItem ever running per-order (e.g.
              -- couldn't fetch the HDFC service config) -- nothing was individually released yet.
              forM_ claimedOrders $ \order ->
                failClaimedOrder order Payout.REJECTED_AT_VALIDATION ("Adhoc bulk submission failed before reaching HDFC: " <> show e)
              pure
                [ case mbOrder of
                    Just _ -> failedItem rp ("Submission failed: " <> show e)
                    Nothing -> skippedItem rp
                  | (rp, mbOrder) <- claimResults
                ]
            Right (excludedOrders, batches) -> do
              let excludedReasonByOrderId = Map.fromList [(order.orderId, reason) | (order, reason) <- excludedOrders]
                  claimTimeExcluded = length group - length claimedOrders
                  submitTimeExcluded = length excludedOrders
                  runTotalAmount = if null batches then Nothing else Just (sum (map (.totalAmount) batches))
              now <- getCurrentTime
              -- Same accounting the scheduled sweep does at assembly-seal time -- the adhoc path
              -- never goes through that seal, so this is the only place these counters get set.
              -- SEALED only when a batch is actually in flight for BulkBatchPayoutPoll to resolve;
              -- if everything got excluded before ever reaching HDFC, there's nothing to poll.
              QPayoutRun.sealRun (if null batches then DDPR.COMPLETED else DDPR.SEALED) runTotalAmount (length batches) (Just now) run.id
              QPayoutRun.updateAssemblyCounts (length group) (claimTimeExcluded + submitTimeExcluded) (length claimedOrders - submitTimeExcluded) run.id
              unless (null batches) $
                scheduleBulkPollJob run merchantId merchantOpCityId payoutServiceName config
              pure
                [ case mbOrder of
                    Nothing -> skippedItem rp
                    Just order -> case Map.lookup order.orderId excludedReasonByOrderId of
                      Just reason -> failedItem rp reason
                      Nothing -> ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.INITIATED, reason = Nothing, payoutOrderId = Just order.orderId}
                  | (rp, mbOrder) <- claimResults
                ]

failedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
failedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.FAILED, reason = Just reason, payoutOrderId = Nothing}

skippedItem :: ResolvedPerson -> ApiPayout.AdhocPayoutResultItem
skippedItem rp = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.SKIPPED, reason = Just "Below minimum payout amount or manually-added VPA", payoutOrderId = Nothing}

-- | Juspay/Stripe people: no batching concept, so each is submitted individually and
--   synchronously -- API B's contract needs a per-person result, unlike the scheduled sweep's
--   fire-and-forget fork via processOneWalletPayout.
processNonBulkGroup :: [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processNonBulkGroup group = forM group $ \rp -> do
  result <- try $ do
    let counterparty = counterpartyFromRole rp.person.role
    now <- getCurrentTime
    mbAccount <- getWalletAccountByOwner counterparty rp.person.id.getId
    walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty rp.person.id.getId
    let timeDiff = secondsToNominalDiffTime rp.transporterConfig.timeDiffFromUtc
        cutoff = payoutCutoffTimeUTC timeDiff rp.transporterConfig.driverWalletConfig.payoutCutOffDays now
    (nonRedeemable, redeemableIds, merchantTransferAmt) <- case (.id) <$> mbAccount of
      Nothing -> pure (0, [], 0)
      Just accountId -> getPayoutEligibilityData accountId cutoff now
    let payoutableBalance = walletBalance - nonRedeemable
        ctx =
          DriverWallet.PayoutContext
            { driverId = rp.person.id,
              merchantId = rp.person.merchantId,
              mocId = rp.merchantOpCity.id,
              person = rp.person,
              payoutVpa = Nothing,
              transporterConfig = rp.transporterConfig
            }
    if rp.isManuallyAdded
      then pure (ApiPayout.SKIPPED, Just "Manually-added VPA", Nothing)
      else
        if payoutableBalance < rp.config.minimumPayoutAmount
          then pure (ApiPayout.SKIPPED, Just ("Below minimum payout amount: " <> show payoutableBalance), Nothing)
          else do
            mbOrder <- DriverWallet.initiateWalletPayout ctx payoutableBalance PR.ADHOC Nothing (Just cutoff) (map (.getId) redeemableIds) merchantTransferAmt
            pure (ApiPayout.INITIATED, Nothing, (.orderId) <$> mbOrder)
  pure $ case result of
    Left (e :: SomeException) -> ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.FAILED, reason = Just (show e), payoutOrderId = Nothing}
    Right (status, reason, orderId) -> ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status, reason, payoutOrderId = orderId}
