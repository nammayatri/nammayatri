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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (groupBy, nub, partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Domain.Action.UI.DriverWallet as DriverWallet
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan (ServiceNames (PREPAID_SUBSCRIPTION))
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Environment
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPRE
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Finance.WalletPayout as WalletPayout
import SharedLogic.Payout.Bulk.Cycle (runBulkPayoutCycle)
import SharedLogic.Payout.Bulk.Eligibility (checkBulkPayoutEligibility, classifyBulkCandidate, usableBankDetails)
import SharedLogic.Payout.Bulk.Types (BulkClaimOutcome (..), PayoutSnapshot (..))
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformation as QFOI
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
    payoutServiceName :: DEMSC.ServiceName
  }

-- | What every person in one city shares for an adhoc payout. Read once per city in a request,
--   not once per person.
data CityPayoutCtx = CityPayoutCtx
  { merchantOpCity :: DMOC.MerchantOperatingCity,
    transporterConfig :: DTConf.TransporterConfig,
    config :: DSPC.ScheduledPayoutConfig,
    payoutServiceFlow :: Payout.PayoutServiceFlow,
    payoutServiceName :: DEMSC.ServiceName
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
--   (source of truth for minimumPayoutAmount/itemsPerBatchLimit/defaultPayoutRail -- isEnabled is
--   deliberately ignored, since adhoc exists to bypass the scheduled-sweep gate).
resolvePerson :: DM.Merchant -> Id.Id DP.Person -> Environment.Flow ResolvedPerson
resolvePerson merchant personId =
  resolvePersons merchant [personId] >>= \case
    [(_, Right rp)] -> pure rp
    [(_, Left e)] -> throwM e
    _ -> throwError (InternalError "resolvePersons returned an unexpected number of results")

-- | 'resolvePerson' for a whole request: one IN query for the people and one set of config reads
--   per distinct city, instead of all of it per person. Each id gets its own result, so one bad id
--   never fails the rest.
resolvePersons :: DM.Merchant -> [Id.Id DP.Person] -> Environment.Flow [(Id.Id DP.Person, Either SomeException ResolvedPerson)]
resolvePersons merchant personIds = do
  persons <- QPerson.findAllByPersonIds (map (.getId) personIds)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
      cityIds = nub [p.merchantOperatingCityId | p <- persons, p.merchantId == merchant.id]
  cities <- forM cityIds $ \mocId -> (,) mocId.getId <$> try (resolveCity mocId)
  let citiesById = Map.fromList cities
      resolveOne personId = do
        person <- maybe (Left (toException (PersonNotFound personId.getId))) Right (Map.lookup personId.getId personsById)
        unless (person.merchantId == merchant.id) $ Left (toException (InvalidRequest "Person does not belong to this merchant"))
        city <- fromMaybe (Left (toException (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId))) (Map.lookup person.merchantOperatingCityId.getId citiesById)
        pure
          ResolvedPerson
            { person,
              merchantOpCity = city.merchantOpCity,
              transporterConfig = city.transporterConfig,
              config = city.config,
              payoutServiceFlow = city.payoutServiceFlow,
              payoutServiceName = city.payoutServiceName
            }
  pure [(pid, resolveOne pid) | pid <- personIds]

resolveCity :: Id.Id DMOC.MerchantOperatingCity -> Environment.Flow CityPayoutCtx
resolveCity mocId = do
  merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  config <-
    getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, isEnabled = Nothing, payoutCategory = Just DPayment.DRIVER_WALLET_TRANSACTION}) Nothing
      >>= fromMaybeM (InvalidRequest "No ScheduledPayoutConfig seeded for this city; seed one via scheduledPayoutConfig/upsert (isEnabled can stay false)")
  (payoutServiceFlow, payoutServiceName) <- TP.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TP.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCity.id
  pure CityPayoutCtx {merchantOpCity, transporterConfig, config, payoutServiceFlow, payoutServiceName}

-- | Compute a person's current wallet balance / payoutable balance for display, before an admin
--   decides to include them in an adhoc initiate call.
lookupPayoutEligibility ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id DP.Person ->
  Environment.Flow ApiPayout.AdhocPayoutLookupResp
lookupPayoutEligibility merchantShortId _opCity personId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  rp <- resolvePerson merchant personId
  let counterparty = counterpartyFromRole rp.person.role
  now <- getCurrentTime
  mbAccount <- getWalletAccountByOwner counterparty personId.getId
  walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty personId.getId
  let timeDiff = secondsToNominalDiffTime rp.transporterConfig.timeDiffFromUtc
      cutoff = payoutCutoffTimeUTC timeDiff rp.transporterConfig.driverWalletConfig.payoutCutOffDays now
  -- Main's hold-aware view: redeemableBalance already excludes what sits in OwnerPayoutLiability
  -- for payouts still in flight, so it cannot be offered twice.
  eligibility <- case (.id) <$> mbAccount of
    Nothing -> pure emptyWalletPayoutEligibility
    Just accountId -> getPayoutEligibilityData counterparty personId.getId accountId walletBalance cutoff now
  let payoutableBalance = eligibility.redeemableBalance
      redeemableIds = eligibility.redeemableEntryIds
      merchantTransferAmt = eligibility.merchantTransferAmount
  -- Read the bank account directly rather than through 'getCreatePayoutServiceFlow', which throws
  -- for a missing or unverified account. That throw is right when money is about to move and wrong
  -- here: this endpoint exists to *describe* a beneficiary, and the ones an admin most needs to
  -- look at are exactly the ones who cannot be paid yet.
  mbBankAccount <- QDBA.findByPrimaryKey personId
  activePayouts <- QPRE.findByBeneficiaryWithFilters personId.getId Nothing Nothing [PR.INITIATED, PR.PROCESSING] (Just 1) (Just 0)
  -- What the bulk rail actually needs: an account row whose number, IFSC and name the bank can use.
  -- Verification status is not consulted -- HDFC never sees it -- so a screen reporting "unverified"
  -- would be describing a beneficiary we would in fact pay.
  let bankAccountStatus = case mbBankAccount of
        Nothing -> "MISSING"
        Just bankAccount
          | isJust (usableBankDetails bankAccount) -> "INCOMPLETE"
          | otherwise -> "PRESENT"
      -- Decide with the same function the initiate path uses, over a snapshot built from the reads
      -- already done above, so the screen can never promise an outcome the initiate would refuse.
      snapshot =
        PayoutSnapshot
          { payoutableBalance = payoutableBalance,
            redeemableEntryIds = map (.getId) redeemableIds,
            merchantTransferAmount = merchantTransferAmt,
            cutoff = cutoff,
            mbBankAccount = mbBankAccount,
            hasPayoutInFlight = not (null activePayouts)
          }
      mbIneligibilityReason = case rp.payoutServiceFlow of
        Payout.BulkFlow -> case classifyBulkCandidate rp.config rp.person snapshot of
          Left reason -> Just reason
          -- An excluded candidate is a batch member whose bank details a human must fix: it reaches
          -- the excluded worklist rather than a payout order, so for display it is not eligible --
          -- and the reason names the field, as the worklist does.
          Right candidate -> candidate.exclusionReason
        -- Juspay is VPA-based and Stripe resolves its own account, so neither is gated on a bank
        -- row here; the amount is the only thing this endpoint can speak to.
        _ ->
          if payoutableBalance >= rp.config.minimumPayoutAmount
            then Nothing
            else Just ("payoutableBalance=" <> show payoutableBalance <> " below minimum=" <> show rp.config.minimumPayoutAmount)
  pure
    ApiPayout.AdhocPayoutLookupResp
      { personId = personId.getId,
        personName = Just rp.person.firstName,
        role = show rp.person.role,
        merchantOperatingCityId = rp.merchantOpCity.id.getId,
        walletBalance = walletBalance,
        nonRedeemableAmount = eligibility.nonRedeemableBalance,
        payoutableBalance = payoutableBalance,
        minimumPayoutAmount = rp.config.minimumPayoutAmount,
        isEligible = isNothing mbIneligibilityReason,
        ineligibilityReason = mbIneligibilityReason,
        payoutServiceFlow = show rp.payoutServiceFlow,
        bankAccountStatus = bankAccountStatus
      }

-- | Push a payout right now for each of the given person ids. One bad/ineligible id never fails
--   the whole request -- each gets its own INITIATED/SKIPPED/FAILED result. BulkFlow (HDFC CBX)
--   people are grouped per (city, flow) into one adhoc payout cycle per group (checks, then the
--   batch, then its rows, then the submission); Juspay/Stripe people are handled individually and
--   synchronously (their contract needs a result before this returns, unlike the scheduled
--   sweep's fire-and-forget fork).
initiateAdhocPayouts ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  [Id.Id DP.Person] ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
initiateAdhocPayouts merchantShortId opCity personIds = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  uniquePersonIds <- validateAdhocRequest merchant opCity personIds
  resolved <- resolvePersons merchant uniquePersonIds
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

-- | Check the request itself, and hand back the de-duplicated ids.
--
--   All of this happens before a single person is looked up, because the point of the cap is to
--   stop the per-person work from being unbounded -- paying for the lookups first would defeat it.
--
--   Three things:
--
--   * An empty list is a caller mistake, not an instruction to pay nobody. It used to answer
--     @{"results": []}@ and quietly do nothing at all.
--   * A repeated id would be resolved and processed twice, so one person would get two result
--     rows. The double-pay guard stops the second payment, but the response would still misreport
--     what happened.
--   * A very long list turns one dashboard click into many batches and many live bank submissions
--     inside a single synchronous request. 'runBulkPayoutCycle' already chunks at the partner's
--     cap, so this is not about producing a valid file -- it is about bounding the request and the
--     blast radius of one mispasted list.
validateAdhocRequest ::
  DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  [Id.Id DP.Person] ->
  Environment.Flow [Id.Id DP.Person]
validateAdhocRequest merchant opCity personIds = do
  when (null personIds) $ throwError (InvalidRequest "personIds is empty")
  let uniquePersonIds = nub personIds
  cap <- adhocPersonIdsCap merchant opCity
  when (length uniquePersonIds > cap) $
    throwError . InvalidRequest $
      "personIds has " <> show (length uniquePersonIds) <> " unique entries; the maximum for this city is " <> show cap
  pure uniquePersonIds

-- | How many people one adhoc call may name: the partner's own batch cap, so a single call is at
--   most a single batch and the two numbers cannot drift apart.
--
--   A payout service with no bulk API -- Juspay, Stripe -- has no such cap, and those people are
--   paid one at a time rather than in a file. They still need bounding (individually is if
--   anything more work per person), so they fall back to a fixed ceiling rather than being
--   refused: 'bulkPartnerCapsOf' returning Nothing is a normal non-bulk merchant here, not an
--   error, which is why this does not reuse the cycle's @fromMaybeM@.
adhocPersonIdsCap :: DM.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow Int
adhocPersonIdsCap merchant opCity = do
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  (_, payoutServiceName) <- TP.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TP.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCity.id
  partner <- TP.getPayoutServiceConfig payoutServiceName merchantOpCity.id
  pure $ maybe nonBulkAdhocCap (.maxItemsPerBatch) (Payout.bulkPartnerCapsOf partner)

-- | Ceiling for a payout service that has no batch of its own to borrow a number from.
nonBulkAdhocCap :: Int
nonBulkAdhocCap = 100

-- | Run one (city, BulkFlow) group of people through a single adhoc HDFC CBX payout cycle: every
--   eligibility check first, then the batch, then the rows underneath it, then the submission.
--   Each person gets their own result, with the real reason when they did not make it in.
--
--   This function never throws, so one group's failure can't take down the rest of the request. A
--   cycle that dies after claiming leaves an unsubmitted batch behind, which the status-check job
--   finds and resolves (it asks HDFC whether the batch arrived, and releases its items when not),
--   so nothing stays reserved with nobody to revisit it.
processBulkGroup :: Id.Id DM.Merchant -> [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processBulkGroup merchantId group = do
  let rp0 = head group
      merchantOpCityId = rp0.merchantOpCity.id
      config = rp0.config
      payoutServiceName = rp0.payoutServiceName
      transporterConfig = rp0.transporterConfig
  checks <- forM group $ \rp -> (,) rp <$> checkBulkPayoutEligibility config transporterConfig rp.person
  let ineligible = [skippedItem rp reason | (rp, Left reason) <- checks]
      eligible = [(rp, candidate) | (rp, Right candidate) <- checks]
  if null eligible
    then pure ineligible
    else do
      cycleResult <-
        try $ runBulkPayoutCycle config payoutServiceName merchantId merchantOpCityId DPayoutBatch.ADHOC PR.ADHOC transporterConfig (map snd eligible)
      case cycleResult of
        Left (e :: SomeException) -> do
          logError $ "Adhoc bulk payout cycle failed for city " <> merchantOpCityId.getId <> ": " <> show e
          pure $ ineligible <> [failedItem rp ("Payout cycle failed: " <> show e) | (rp, _) <- eligible]
        Right outcomes -> do
          let outcomeByPerson = Map.fromList [(candidate.person.id.getId, outcome) | (candidate, outcome) <- outcomes]
          -- Nothing else to schedule: each batch carries the time of its first status check, and
          -- the always-on status-check job picks it up from there.
          pure $
            ineligible
              <> [ case Map.lookup rp.person.id.getId outcomeByPerson of
                     Just (ClaimSubmitted order) ->
                       ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.INITIATED, reason = Nothing, payoutOrderId = Just order.orderId}
                     Just (ClaimExcluded reason) -> skippedItem rp ("Excluded: " <> reason)
                     Just (ClaimDropped reason) -> skippedItem rp reason
                     Nothing -> skippedItem rp "Not processed in this payout cycle"
                   | (rp, _) <- eligible
                 ]

failedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
failedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.FAILED, reason = Just reason, payoutOrderId = Nothing}

-- | Not paid, and nothing was attempted: the reason is the one the eligibility check actually
--   gave, not a catch-all -- an admin acts on "no bank details on file" quite differently from
--   "below minimum payout amount".
skippedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
skippedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.SKIPPED, reason = Just reason, payoutOrderId = Nothing}

-- | Juspay/Stripe people: no batching concept, so each is submitted individually and
--   synchronously -- API B's contract needs a per-person result, unlike the scheduled sweep's
--   fire-and-forget fork via processOneWalletPayout.
processNonBulkGroup :: [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processNonBulkGroup group = forM group $ \rp -> do
  -- Same per-person lock the bulk claim takes (SharedLogic.Payout.Bulk.Claim), so an adhoc
  -- initiate and the scheduled sweep cannot both decide to pay the same beneficiary: the
  -- in-flight check below is a read followed by a write, and without this they can interleave
  -- between the two and both proceed. The hold posted inside is the second line of defence --
  -- once it is in place the balance no longer shows the money as payable.
  -- 'withWaitOnLockRedisWithExpiry' returns (), so the outcome comes back through an IORef --
  -- the same shape SharedLogic.Payout.Bulk.Claim uses for exactly this reason. The default stands
  -- for "the lock was never entered", which is what a contended wait leaves behind.
  outcomeRef <- liftIO $ newIORef (ApiPayout.SKIPPED, Just "Another payout for this beneficiary is in progress", Nothing)
  result <- try $
    Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey rp.person.id.getId) 10 10 $ do
      let counterparty = counterpartyFromRole rp.person.role
      now <- getCurrentTime
      mbAccount <- getWalletAccountByOwner counterparty rp.person.id.getId
      walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty rp.person.id.getId
      let timeDiff = secondsToNominalDiffTime rp.transporterConfig.timeDiffFromUtc
          cutoff = payoutCutoffTimeUTC timeDiff rp.transporterConfig.driverWalletConfig.payoutCutOffDays now
      eligibility <- case (.id) <$> mbAccount of
        Nothing -> pure emptyWalletPayoutEligibility
        Just accountId -> getPayoutEligibilityData counterparty rp.person.id.getId accountId walletBalance cutoff now
      let payoutableBalance = eligibility.redeemableBalance
          redeemableIds = eligibility.redeemableEntryIds
          merchantTransferAmt = eligibility.merchantTransferAmount
          ctx =
            WalletPayout.PayoutContext
              { driverId = rp.person.id,
                merchantId = rp.person.merchantId,
                mocId = rp.merchantOpCity.id,
                person = rp.person,
                payoutVpa = Nothing,
                transporterConfig = rp.transporterConfig
              }
      -- Double-pay guard (§6.1): skip if a payout is already in flight for this beneficiary
      -- (amount is derived from wallet balance, which isn't decremented until settlement).
      activePayouts <- QPRE.findByBeneficiaryWithFilters rp.person.id.getId Nothing Nothing [PR.INITIATED, PR.PROCESSING] (Just 1) (Just 0)
      outcome <-
        if not (null activePayouts)
          then pure (ApiPayout.SKIPPED, Just "In-flight payout request already exists (double-pay guard §6.1)", Nothing)
          else do
            -- Only the non-bulk rails care about a manually added VPA, so it is looked up here.
            isManuallyAdded <- resolveIsManuallyAdded rp.person
            if isManuallyAdded
              then pure (ApiPayout.SKIPPED, Just "Manually-added VPA", Nothing)
              else
                if payoutableBalance < rp.config.minimumPayoutAmount
                  then pure (ApiPayout.SKIPPED, Just ("Below minimum payout amount: " <> show payoutableBalance), Nothing)
                  else do
                    mbOrder <- DriverWallet.initiateWalletPayout ctx payoutableBalance PR.ADHOC Nothing (Just cutoff) (map (.getId) redeemableIds) merchantTransferAmt Nothing Nothing afterPayoutOrderCreated
                    pure (ApiPayout.INITIATED, Nothing, (.orderId) <$> mbOrder)
      liftIO $ writeIORef outcomeRef outcome
  case result of
    Left (e :: SomeException) -> pure ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.FAILED, reason = Just (show e), payoutOrderId = Nothing}
    Right () -> do
      (status, reason, orderId) <- liftIO $ readIORef outcomeRef
      pure ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status, reason, payoutOrderId = orderId}
