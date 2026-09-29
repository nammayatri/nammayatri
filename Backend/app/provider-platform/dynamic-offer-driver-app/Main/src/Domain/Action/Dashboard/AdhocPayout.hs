{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): New file; main has no adhoc payout API. Backs GET payout/adhoc/lookup and
--   POST payout/adhoc/initiate. An admin pays named people right now through one bulk cycle for the URL's city
--   (checks -> open batch -> claim -> submit); the city's status-check job then settles the batch like a sweep batch.
--   Bulk-only: in a Juspay/Stripe city both endpoints answer 400 before any person is looked up. Every setting comes from
--   the URL's city, and a person from another city is refused. No existing Juspay/Stripe behaviour changes.
module Domain.Action.Dashboard.AdhocPayout
  ( lookupPayoutEligibility,
    initiateAdhocPayouts,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import Control.Applicative ((<|>))
import Data.List (nub, partition)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan (ServiceNames (PREPAID_SUBSCRIPTION))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
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
import Lib.Payment.Payout.Bulk.Types (BulkClaimOutcome (..))
import SharedLogic.Finance.Wallet
import SharedLogic.Payout.Bulk.Driver (runBulkPayoutCycle)
import SharedLogic.Payout.Bulk.Eligibility (PayoutSnapshot (..), checkBulkPayoutEligibility, classifyBulkCandidate, usableBankDetails)
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

-- | Resolve + validate one person for an adhoc payout: must exist, must belong to the
--   dashboard-authenticated merchant and to the city in the URL. Everything else comes from that
--   city (see 'resolveUrlCity').
resolvePerson :: DM.Merchant -> CityPayoutCtx -> Id.Id DP.Person -> Environment.Flow ResolvedPerson
resolvePerson merchant city personId =
  resolvePersons merchant city [personId] >>= \case
    [(_, Right rp)] -> pure rp
    [(_, Left e)] -> throwM e
    _ -> throwError (InternalError "resolvePersons returned an unexpected number of results")

-- NOTE (reviewer, remove before merge): The person must belong to the dashboard's merchant and to the URL's city, judged
--   by Person.merchantOperatingCityId, and everyone is paid with the URL city's settings. (The sweep picks people by the
--   DriverInformation / FleetOwnerInformation city; normally the same city.) Bulk-only.

-- | 'resolvePerson' for a whole request: one IN query for the people. Everyone uses the URL city's
--   settings, and a person from any other city is refused. Each id gets its own result, so one bad
--   id never fails the rest.
resolvePersons :: DM.Merchant -> CityPayoutCtx -> [Id.Id DP.Person] -> Environment.Flow [(Id.Id DP.Person, Either SomeException ResolvedPerson)]
resolvePersons merchant city personIds = do
  persons <- QPerson.findAllByPersonIds (map (.getId) personIds)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
      resolveOne personId = do
        person <- maybe (Left (toException (PersonNotFound personId.getId))) Right (Map.lookup personId.getId personsById)
        unless (person.merchantId == merchant.id) $ Left (toException (InvalidRequest "Person does not belong to this merchant"))
        unless (person.merchantOperatingCityId == city.merchantOpCity.id) $ Left (toException (InvalidRequest "Person is not in this city"))
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

-- NOTE (reviewer, remove before merge): The bulk-only gate. Both adhoc endpoints call it first, so a Juspay/Stripe city
--   is refused here. The city also needs a ScheduledPayoutConfig row (disabled is fine), else 400.
--   resolveCity reads that config through config-pilot before the BulkFlow check, with the same key the sweep and the
--   upsert use. Called inside the KV drain lag right after a brand-new config is created, it can cache "no config" for
--   up to 2 h, or until the next save clears it. The call itself writes and pays nothing. Low.

-- | The city in the URL, with everything a payout there needs. Adhoc payouts are bulk-only: the
--   city must pay through a bulk payout partner (HDFC CBX); any other city is refused. The city must
--   also have a ScheduledPayoutConfig row seeded (source of truth for minimumPayoutAmount/
--   itemsPerBatchLimit/defaultPayoutRail -- isEnabled is deliberately ignored, since adhoc exists to
--   bypass the scheduled-sweep gate).
resolveUrlCity :: DM.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow CityPayoutCtx
resolveUrlCity merchant opCity = do
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  city <- resolveCity merchantOpCity.id
  unless (city.payoutServiceFlow == Payout.BulkFlow) $
    throwError . InvalidRequest $
      "Adhoc payouts are only available in a city that pays through a bulk payout partner; this city uses " <> show city.payoutServiceFlow
  pure city

resolveCity :: Id.Id DMOC.MerchantOperatingCity -> Environment.Flow CityPayoutCtx
resolveCity mocId = do
  merchantOpCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  config <-
    getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, isEnabled = Nothing, payoutCategory = Just DPayment.DRIVER_WALLET_TRANSACTION}) Nothing
      >>= fromMaybeM (InvalidRequest "No ScheduledPayoutConfig seeded for this city; seed one via scheduledPayoutConfig/upsert (isEnabled can stay false)")
  (payoutServiceFlow, payoutServiceName) <- TP.getPayoutServiceFlowForMerchant (.createPayoutOrder) (TP.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService merchantOpCity.id
  pure CityPayoutCtx {merchantOpCity, transporterConfig, config, payoutServiceFlow, payoutServiceName}

-- NOTE (reviewer, remove before merge): Read-only lookup screen; writes nothing. The amount comes from
--   computePayoutableBalance (the same function WalletPayout.findWalletPayoutAmount uses for Juspay/Stripe), and the answer
--   uses the same gates as initiate. bankAccountStatus only checks that usable bank details are present (MISSING /
--   INCOMPLETE / PRESENT). Bulk-only.

-- | Compute a person's current wallet balance / payoutable balance for display, before an admin
--   decides to include them in an adhoc initiate call.
lookupPayoutEligibility ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id DP.Person ->
  Environment.Flow ApiPayout.AdhocPayoutLookupResp
lookupPayoutEligibility merchantShortId opCity personId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  city <- resolveUrlCity merchant opCity
  rp <- resolvePerson merchant city personId
  let counterparty = counterpartyFromRole rp.person.role
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime rp.transporterConfig.timeDiffFromUtc
      cutoff = payoutCutoffTimeUTC timeDiff rp.transporterConfig.driverWalletConfig.payoutCutOffDays now
  -- The same payable amount the initiate path pays: redeemable less ride and offer holds. The
  -- redeemable balance already excludes what sits in OwnerPayoutLiability for payouts still in
  -- flight, so it cannot be offered twice.
  pb <- computePayoutableBalance counterparty personId.getId cutoff now
  let eligibility = pb.eligibility
      walletBalance = pb.walletBalance
      payoutableBalance = pb.payoutableBalance
      redeemableIds = eligibility.redeemableEntryIds
      merchantTransferAmt = eligibility.merchantTransferAmount
  -- Read the bank account directly rather than through 'getCreatePayoutServiceFlow', which throws
  -- for a missing account. That throw is right when money is about to move and wrong
  -- here: this endpoint exists to *describe* a beneficiary, and the ones an admin most needs to
  -- look at are exactly the ones who cannot be paid yet.
  mbBankAccount <- QDBA.findByPrimaryKey personId
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
            mbBankAccount = mbBankAccount
          }
      classifyReason = case classifyBulkCandidate rp.config rp.person snapshot of
        Left reason -> Just reason
        -- An excluded candidate is a batch member whose bank details a human must fix: it reaches
        -- the excluded worklist rather than a payout order, so for display it is not eligible --
        -- and the reason names the field, as the worklist does.
        Right candidate -> candidate.exclusionReason
  -- The same gates the initiate path applies before anything else, so the screen never shows as
  -- payable someone the initiate would turn away.
  blockReasons <- personBlockReasons [rp.person]
  let mbIneligibilityReason =
        (if walletPayoutsEnabled merchant rp.transporterConfig then Nothing else Just walletPayoutsDisabledReason)
          <|> Map.lookup rp.person.id.getId blockReasons
          <|> classifyReason
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

-- | Push a payout right now for each of the given person ids, through one bulk payout cycle for the
--   URL's city (checks, then the batch, then its rows, then the submission). Bulk-only: a city that
--   doesn't pay through a bulk payout partner is refused. One bad/ineligible id never fails the
--   whole request -- each gets its own INITIATED/SKIPPED/FAILED result.
initiateAdhocPayouts ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  [Id.Id DP.Person] ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
initiateAdhocPayouts merchantShortId opCity personIds = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  city <- resolveUrlCity merchant opCity
  -- The city's wallet payout switch, checked as the scheduled sweep checks it: when ops have turned
  -- payouts off, an admin cannot push one through by hand either.
  unless (walletPayoutsEnabled merchant city.transporterConfig) $
    throwError (InvalidRequest walletPayoutsDisabledReason)
  uniquePersonIds <- validateAdhocRequest city personIds
  resolved <- resolvePersons merchant city uniquePersonIds
  let failureResults =
        [ ApiPayout.AdhocPayoutResultItem {personId = pid.getId, status = ApiPayout.FAILED, reason = Just (show e), payoutOrderId = Nothing}
          | (pid, Left e) <- resolved
        ]
      people = [rp | (_, Right rp) <- resolved]
  bulkResults <- if null people then pure [] else processBulkGroup merchant.id city people
  pure $ ApiPayout.AdhocPayoutInitiateResp (failureResults <> bulkResults)

-- NOTE (reviewer, remove before merge): The cap is the bulk partner's maxItemsPerBatch for the URL's city. Bulk-only.

-- | Check the request itself, and hand back the de-duplicated ids.
--
--   All of this happens before a single person is looked up, because the point of the cap is to
--   stop the per-person work from being unbounded -- paying for the lookups first would defeat it.
--
--   Three things:
--
--   * An empty list is a caller mistake, not an instruction to pay nobody.
--   * A repeated id would be resolved and processed twice, so one person would get two result
--     rows. The wallet lock and hold in the claim stop the second payment, but the response would
--     still misreport what happened.
--   * A very long list turns one dashboard click into many batches and many live bank submissions
--     inside a single synchronous request. 'runBulkPayoutCycle' already chunks at the partner's
--     cap, so this is not about producing a valid file -- it is about bounding the request and the
--     blast radius of one mispasted list.
validateAdhocRequest ::
  CityPayoutCtx ->
  [Id.Id DP.Person] ->
  Environment.Flow [Id.Id DP.Person]
validateAdhocRequest city personIds = do
  when (null personIds) $ throwError (InvalidRequest "personIds is empty")
  let uniquePersonIds = nub personIds
  cap <- adhocPersonIdsCap city
  when (length uniquePersonIds > cap) $
    throwError . InvalidRequest $
      "personIds has " <> show (length uniquePersonIds) <> " unique entries; the maximum for this city is " <> show cap
  pure uniquePersonIds

-- | How many people one adhoc call may name: the partner's own batch cap, so the two numbers cannot
--   drift apart. A lower itemsPerBatchLimit for the city can still split one call into several batches.
adhocPersonIdsCap :: CityPayoutCtx -> Environment.Flow Int
adhocPersonIdsCap city = do
  partner <- TP.getPayoutServiceConfig city.payoutServiceName city.merchantOpCity.id
  caps <-
    Payout.bulkPartnerCapsOf partner
      & fromMaybeM (InvalidRequest $ "Payout service " <> show city.payoutServiceName <> " is not a bulk payout partner")
  pure caps.maxItemsPerBatch

-- | Run the people of one adhoc call through a single HDFC CBX payout cycle for the URL's city:
--   every eligibility check first, then the batch, then the rows underneath it, then the submission.
--   Each person gets their own result, with the real reason when they did not make it in.
--
--   This function never throws: a cycle failure becomes a FAILED result per person. A cycle that dies
--   after claiming leaves an unsubmitted batch behind, which the city's status-check job finds: it
--   asks HDFC whether the batch arrived and checks it like any other if so; if HDFC never had it,
--   the batch ends in manual review with its money still held, for ops to sort out by hand.
processBulkGroup :: Id.Id DM.Merchant -> CityPayoutCtx -> [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processBulkGroup merchantId city group = do
  let merchantOpCityId = city.merchantOpCity.id
      config = city.config
      payoutServiceName = city.payoutServiceName
      transporterConfig = city.transporterConfig
  -- People ops have disabled or blocked are skipped before any balance is read, as the sweep's
  -- eligibility queries leave them out.
  blockReasons <- personBlockReasons (map (.person) group)
  let (blocked, allowed) = partition (\rp -> Map.member rp.person.id.getId blockReasons) group
      blockedItems = [skippedItem rp reason | rp <- blocked, Just reason <- [Map.lookup rp.person.id.getId blockReasons]]
  checks <- forM allowed $ \rp -> (,) rp <$> checkBulkPayoutEligibility config transporterConfig rp.person
  let ineligible = blockedItems <> [skippedItem rp reason | (rp, Left reason) <- checks]
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
          let outcomeByPerson = Map.fromList [(candidate.beneficiary.id.getId, outcome) | (candidate, outcome) <- outcomes]
          -- Nothing else to schedule: each batch carries the time of its first status check, and
          -- the city's status-check job, made when the batch was opened, picks it up from there.
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

-- NOTE (reviewer, remove before merge): These two helpers give adhoc the scheduled sweep's own gates, so an admin cannot
--   pay while the city's wallet payouts are off, or pay a disabled / blocked person. The switch is the sweep's walletEnabled
--   (ScheduledBatchPayout.processWalletPayouts); the person flags are the filters of
--   DriverInformationExtra.findEligibleForScheduledPayout / FleetOwnerInformationExtra.findEligibleFleetOwnersForScheduledPayout.

-- | Whether the city pays wallet payouts at all -- the scheduled sweep's own switch.
walletPayoutsEnabled :: DM.Merchant -> DTConf.TransporterConfig -> Bool
walletPayoutsEnabled merchant transporterConfig =
  fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableWalletPayout

walletPayoutsDisabledReason :: Text
walletPayoutsDisabledReason = "Wallet payouts are disabled for this city"

-- | Why each of these people may not be paid, for the ones who may not: the flags the scheduled
--   sweep filters on -- the account is enabled, not blocked, and not blocked for scheduled payouts.
--   Drivers are read from driver_information, fleet owners from fleet_owner_information, one query
--   each; someone with no row there is not someone the sweep would pay either.
personBlockReasons :: [DP.Person] -> Environment.Flow (Map.Map Text Text)
personBlockReasons persons = do
  let isFleetOwner p = p.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]
      (fleetOwners, drivers) = partition isFleetOwner persons
  driverInfos <- if null drivers then pure [] else QDI.findAllByDriverIds (map (.id.getId) drivers)
  fleetOwnerInfos <- QFOI.findAllByFleetOwnerPersonIds (map (.id.getId) fleetOwners)
  let driverFlags = Map.fromList [(di.driverId.getId, (di.enabled, di.blocked, di.isBlockedForScheduledPayout)) | di <- driverInfos]
      fleetOwnerFlags = Map.fromList [(foi.fleetOwnerPersonId.getId, (foi.enabled, foi.blocked, foi.isBlockedForScheduledPayout)) | foi <- fleetOwnerInfos]
      reasonFor p =
        let (flags, missing)
              | isFleetOwner p = (Map.lookup p.id.getId fleetOwnerFlags, "No fleet owner record")
              | otherwise = (Map.lookup p.id.getId driverFlags, "No driver record")
         in maybe (Just missing) blockReason flags
  pure $ Map.fromList [(p.id.getId, reason) | p <- persons, Just reason <- [reasonFor p]]
  where
    blockReason (enabled, blocked, blockedForPayout)
      | not enabled = Just "Account is disabled"
      | blocked = Just "Account is blocked"
      | blockedForPayout == Just True = Just "Account is blocked for payouts"
      | otherwise = Nothing

failedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
failedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.FAILED, reason = Just reason, payoutOrderId = Nothing}

-- | Not paid, and nothing was attempted: the reason is the one the eligibility check actually
--   gave, not a catch-all -- an admin acts on "no bank details on file" quite differently from
--   "below minimum payout amount".
skippedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
skippedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.SKIPPED, reason = Just reason, payoutOrderId = Nothing}
