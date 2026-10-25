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
import Control.Applicative ((<|>))
import Data.List (nub, partition)
import qualified Data.Map.Strict as Map
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Extra.Plan (ServiceNames (PREPAID_SUBSCRIPTION))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import qualified Environment
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation (Validate, runRequestValidation, validateField)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Domain.Types.Account as FAccount
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

-- | A person checked for an adhoc payout, with their city's payout settings.
data ResolvedPerson = ResolvedPerson
  { person :: DP.Person,
    merchantOpCity :: DMOC.MerchantOperatingCity,
    transporterConfig :: DTConf.TransporterConfig,
    config :: DSPC.ScheduledPayoutConfig,
    payoutServiceFlow :: Payout.PayoutServiceFlow,
    payoutServiceName :: DEMSC.ServiceName
  }

-- | Payout settings shared by everyone in one city, read once per request.
data CityPayoutCtx = CityPayoutCtx
  { merchantOpCity :: DMOC.MerchantOperatingCity,
    transporterConfig :: DTConf.TransporterConfig,
    config :: DSPC.ScheduledPayoutConfig,
    payoutServiceFlow :: Payout.PayoutServiceFlow,
    payoutServiceName :: DEMSC.ServiceName
  }

-- | Resolve one person, who must belong to this merchant and to the URL's city.
resolvePerson :: DM.Merchant -> CityPayoutCtx -> Id.Id DP.Person -> Environment.Flow ResolvedPerson
resolvePerson merchant city personId =
  resolvePersons merchant city [personId] >>= \case
    [(_, Right rp)] -> pure rp
    [(_, Left e)] -> throwM e
    _ -> throwError (InternalError "resolvePersons returned an unexpected number of results")

-- | 'resolvePerson' for a whole request in one query. A bad id fails only its own result.
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

-- | The person a mobile number belongs to in this merchant and city, among the roles adhoc pays.
--   A number on two accounts (a driver who is also a fleet owner) is refused rather than guessed.
findPersonIdByMobileNumber :: DM.Merchant -> CityPayoutCtx -> Maybe Text -> Maybe Text -> Environment.Flow (Id.Id DP.Person)
findPersonIdByMobileNumber merchant city mbMobileNumber mbMobileCountryCode = do
  mobileNumber <- fromMaybeM (InvalidRequest "Provide personId or mobileNumber") mbMobileNumber
  let country = city.merchantOpCity.country
      mobileCountryCode = fromMaybe (P.getCountryMobileCode country) (DCommon.appendPlusInMobileCountryCode mbMobileCountryCode)
  runRequestValidation (validateMobileLookup country) (mobileNumber, mobileCountryCode)
  mobileNumberHash <- getDbHash mobileNumber
  persons <- QPerson.findAllByMobileNumberAndMerchantAndRoles mobileCountryCode mobileNumberHash merchant.id [DP.DRIVER, DP.FLEET_OWNER, DP.FLEET_BUSINESS]
  case filter (\p -> p.merchantOperatingCityId == city.merchantOpCity.id) persons of
    [person] -> pure person.id
    [] -> throwError (InvalidRequest "No driver or fleet owner with this mobile number in this city")
    _ -> throwError (InvalidRequest "This mobile number belongs to more than one account; look up by personId")

-- | The same per-country phone rules the dashboard's registration APIs apply.
validateMobileLookup :: Kernel.Types.Beckn.Context.Country -> Validate (Text, Text)
validateMobileLookup country (mobileNumber, mobileCountryCode) =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber (P.getMobileNumberPredicate country),
      validateField "mobileCountryCode" mobileCountryCode (P.getMobileCountryCodePredicate country)
    ]

-- | The URL's city and its payout settings. Adhoc is bulk-only (HDFC CBX); other cities are refused.
--   Its ScheduledPayoutConfig must be seeded, but isEnabled is ignored: adhoc exists to bypass the sweep.
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

-- | A person's balance and whether they can be paid now, shown before an admin initiates a payout.
lookupPayoutEligibility ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe (Id.Id DP.Person) ->
  Maybe Text -> -- mobileNumber, used when there is no personId
  Maybe Text -> -- mobileCountryCode, the city's country code when absent
  Environment.Flow ApiPayout.AdhocPayoutLookupResp
lookupPayoutEligibility merchantShortId opCity mbPersonId mbMobileNumber mbMobileCountryCode = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  city <- resolveUrlCity merchant opCity
  personId <- maybe (findPersonIdByMobileNumber merchant city mbMobileNumber mbMobileCountryCode) pure mbPersonId
  rp <- resolvePerson merchant city personId
  let counterparty = counterpartyFromRole rp.person.role
  -- Same amount the initiate path pays. Money held for in-flight payouts is already
  -- excluded; the redeemable cutoff comes from the kernel class config via the breakdown.
  pb <- computePayoutableBalance counterparty personId.getId
  let eligibility = pb.eligibility
      walletBalance = pb.walletBalance
      payoutableBalance = pb.payoutableBalance
      redeemableIds = eligibility.redeemableEntryIds
      merchantTransferAmt = eligibility.merchantTransferAmount
  -- Read the account directly: 'getCreatePayoutServiceFlow' throws when it is missing, and this
  -- screen must still describe people who cannot be paid yet.
  mbBankAccount <- QDBA.findByPrimaryKey personId
  -- Verification status is ignored because HDFC never sees it; only usable details matter.
  let bankAccountStatus = case mbBankAccount of
        Nothing -> "MISSING"
        Just bankAccount
          | isJust (usableBankDetails bankAccount) -> "INCOMPLETE"
          | otherwise -> "PRESENT"
      -- Same check the initiate path runs, so this screen never promises what initiate would refuse.
      snapshot =
        PayoutSnapshot
          { payoutableBalance = payoutableBalance,
            redeemableEntryIds = map (.getId) redeemableIds,
            merchantTransferAmount = merchantTransferAmt,
            cutoff = pb.resolvedCutoff,
            mbBankAccount = mbBankAccount
          }
      classifyReason = case classifyBulkCandidate rp.config rp.person snapshot of
        Left reason -> Just reason
        -- Excluded means a human must fix bank details first, so it shows as not eligible.
        Right candidate -> candidate.exclusionReason
  -- Same account blocks the initiate path applies.
  blockReasons <- personBlockReasons [rp.person]
  payoutsEnabled <- walletPayoutsEnabled merchant rp.transporterConfig (counterpartyFromRole rp.person.role)
  let mbIneligibilityReason =
        (if payoutsEnabled then Nothing else Just walletPayoutsDisabledReason)
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

-- | Pay each given person now through one bulk cycle for the URL's city.
--   Each id gets its own INITIATED/SKIPPED/FAILED result, so one bad id never fails the request.
initiateAdhocPayouts ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  [Id.Id DP.Person] ->
  Environment.Flow ApiPayout.AdhocPayoutInitiateResp
initiateAdhocPayouts merchantShortId opCity personIds = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  city <- resolveUrlCity merchant opCity
  uniquePersonIds <- validateAdhocRequest city personIds
  resolved <- resolvePersons merchant city uniquePersonIds
  -- Gate each person on THEIR class (DRIVER vs FLEET_OWNER) — the same gate the kernel
  -- applies at initiate — so an admin cannot push a disabled class through by hand, and
  -- one disabled class never blocks the other.
  gated <- forM resolved $ \case
    (pid, Left e) -> pure (pid, Left (show e))
    (pid, Right rp) -> do
      enabled <- walletPayoutsEnabled merchant rp.transporterConfig (counterpartyFromRole rp.person.role)
      pure (pid, if enabled then Right rp else Left walletPayoutsDisabledReason)
  let failureResults =
        [ ApiPayout.AdhocPayoutResultItem {personId = pid.getId, status = ApiPayout.FAILED, reason = Just reason, payoutOrderId = Nothing}
          | (pid, Left reason) <- gated
        ]
      people = [rp | (_, Right rp) <- gated]
  bulkResults <- if null people then pure [] else processBulkGroup merchant.id city people
  pure $ ApiPayout.AdhocPayoutInitiateResp (failureResults <> bulkResults)

-- | Reject an empty or oversized list and drop duplicate ids, before any per-person lookup.
--   A duplicate would give one person two result rows; the cap bounds what one mispasted list sends.
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

-- | Max ids per call: one batch's worth (partner cap, or the city's lower itemsPerBatchLimit).
--   One call is then one batch, so the reply never mixes a submitted batch with a failed one.
adhocPersonIdsCap :: CityPayoutCtx -> Environment.Flow Int
adhocPersonIdsCap city = do
  partner <- TP.getPayoutServiceConfig city.payoutServiceName city.merchantOpCity.id
  caps <-
    Payout.bulkPartnerCapsOf partner
      & fromMaybeM (InvalidRequest $ "Payout service " <> show city.payoutServiceName <> " is not a bulk payout partner")
  pure $ max 1 (maybe caps.maxItemsPerBatch (min caps.maxItemsPerBatch) city.config.itemsPerBatchLimit)

-- | Run one call's people through a single HDFC CBX cycle. Never throws: a cycle failure is FAILED per person.
--   A cycle that dies after claiming leaves its batch to the status-check job; if HDFC never got it,
--   it ends in manual review with the money held until an admin releases it.
processBulkGroup :: Id.Id DM.Merchant -> CityPayoutCtx -> [ResolvedPerson] -> Environment.Flow [ApiPayout.AdhocPayoutResultItem]
processBulkGroup merchantId city group = do
  let merchantOpCityId = city.merchantOpCity.id
      config = city.config
      payoutServiceName = city.payoutServiceName
      transporterConfig = city.transporterConfig
  -- Skip disabled or blocked people before reading balances, as the sweep does.
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
          -- Nothing to schedule: the city's status-check job picks the batch up at its first check time.
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

-- | The wallet payout switch for the class of the person BEING PAID — the same class
--   the kernel's initiateTransferOut gates on, so this pre-check can never disagree
--   with it (a fleet owner is gated by the FLEET_OWNER row, a driver by DRIVER).
--   (merchant.prepaidSubscriptionAndWalletEnabled gates the prepaid billing product and
--   wallet-ledger emission, not payouts; prepaid-stack cities get their class rows from
--   seed migration 0015.)
walletPayoutsEnabled :: DM.Merchant -> DTConf.TransporterConfig -> FAccount.CounterpartyType -> Environment.Flow Bool
walletPayoutsEnabled merchant transporterConfig counterparty = do
  cfg <- walletClassConfig merchant.id.getId transporterConfig.merchantOperatingCityId.getId counterparty
  pure cfg.transferOutEnabled

walletPayoutsDisabledReason :: Text
walletPayoutsDisabledReason = "Wallet payouts are disabled for this city"

-- | Why each blocked person may not be paid, using the sweep's flags (enabled, blocked, blocked for payouts).
--   Someone with no driver or fleet owner info row is refused too, as the sweep would skip them.
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

-- | Not paid and nothing attempted. Carries the check's real reason, since admins act on it.
skippedItem :: ResolvedPerson -> Text -> ApiPayout.AdhocPayoutResultItem
skippedItem rp reason = ApiPayout.AdhocPayoutResultItem {personId = rp.person.id.getId, status = ApiPayout.SKIPPED, reason = Just reason, payoutOrderId = Nothing}
