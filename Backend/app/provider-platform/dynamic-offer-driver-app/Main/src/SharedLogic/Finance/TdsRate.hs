-- |
-- Single owner of a person's stored TDS rate.
--
-- Two invariants live here, both learned the hard way:
--
--   1. @driver_information.tds_rate@ / @fleet_owner_information.tds_rate@ is the
--      authoritative rate used at ride end, and it must always hold a
--      cohort-derived value for that person's /current/ PAN state. Anything that
--      changes PAN validity, PAN type or Aadhaar linkage has to re-materialise it,
--      otherwise the column silently keeps answering for a state that no longer
--      exists.
--
--   2. A rate is a decimal FRACTION, at most the merchant's own
--      @invalidPanTdsRate@ (the 206AA penal rate, the worst case any cohort
--      branch can produce): 0.001 is 0.1%, 0.20 is 20%. A stored
--      12 was read as 1200% and over-deducted Rs 16,541 from one fleet owner across
--      five rides. Every write goes through 'setTdsRateValidatedWith' so that cannot
--      recur, whatever the caller.
--
-- Note that 'materializeTdsRateFor' and 'ensureTdsRateFor' deliberately recompute
-- from the cohort with 'Nothing' as the stored rate, rather than letting the
-- existing column participate. That is what makes re-materialisation able to
-- /correct/ a wrong stored value instead of preserving it.
module SharedLogic.Finance.TdsRate
  ( isValidTdsRate,
    assertValidTdsRateFor,
    assertValidTdsRate,
    maxAllowedTdsRate,
    setTdsRateValidatedWith,
    setTdsRateValidatedFor,
    materializeTdsRateFor,
    ensureTdsRateFor,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.Finance.Wallet as Wallet
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.FleetOwnerInformation as QFOI

maxAllowedTdsRate :: DTC.TaxConfig -> Double
maxAllowedTdsRate taxConfig = taxConfig.invalidPanTdsRate.rate

isValidTdsRate :: DTC.TaxConfig -> Double -> Bool
isValidTdsRate taxConfig r = r >= 0 && r <= maxAllowedTdsRate taxConfig

-- Local so this module depends only on Domain.Types.Person; mirrors
-- SharedLogic.DriverOnboarding.isFleetRole, which lives behind a much heavier
-- import graph.
isFleetOwnerRole :: Person.Role -> Bool
isFleetOwnerRole Person.FLEET_OWNER = True
isFleetOwnerRole Person.FLEET_BUSINESS = True
isFleetOwnerRole _ = False

-- | The only sanctioned way to write the column. Rejects an out-of-range rate
-- outright: a bad rate is worse than no rate, because the ride path trusts
-- whatever is stored here over the cohort.
setTdsRateValidatedFor ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Bool ->
  Maybe Double ->
  m ()
setTdsRateValidatedFor merchantOpCityId personId isFleet mbRate = do
  transporterConfig <- getTransporterConfigFor merchantOpCityId
  setTdsRateValidatedWith transporterConfig.taxConfig personId isFleet mbRate

-- | Validate without writing, for callers that must reject a bad rate /before/
-- committing other state. The document-approval flows persist the document and
-- only then set the rate, so a throw from the setter would otherwise leave an
-- approved document behind -- doApproveWithRevert reverts the image status, not
-- the document row.
assertValidTdsRateFor ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Maybe Double ->
  m ()
assertValidTdsRateFor merchantOpCityId personId mbRate =
  whenJust mbRate $ \rate -> do
    transporterConfig <- getTransporterConfigFor merchantOpCityId
    assertValidTdsRate transporterConfig.taxConfig personId rate

assertValidTdsRate :: (MonadFlow m) => DTC.TaxConfig -> Id Person.Person -> Double -> m ()
assertValidTdsRate taxConfig personId rate =
  unless (isValidTdsRate taxConfig rate) $
    throwError $
      InvalidRequest $
        "TDS rate must be a decimal fraction between 0 and "
          <> show (maxAllowedTdsRate taxConfig)
          <> " (the invalid-PAN rate; 0.001 = 0.1%), got: "
          <> show rate
          <> " for person "
          <> personId.getId

setTdsRateValidatedWith ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DTC.TaxConfig ->
  Id Person.Person ->
  Bool ->
  Maybe Double ->
  m ()
setTdsRateValidatedWith taxConfig personId isFleet mbRate = do
  whenJust mbRate $ assertValidTdsRate taxConfig personId
  if isFleet
    then QFOI.updateTdsRate mbRate personId
    else QDI.updateTdsRate mbRate personId

getTransporterConfigFor :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DTC.TransporterConfig
getTransporterConfigFor merchantOpCityId =
  getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
    >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

-- | Recompute this person's rate from their current PAN state and store it.
--
-- No-op unless the merchant has PAN-Aadhaar-link TDS enabled, so merchants still
-- on the legacy rate resolution are untouched.
--
-- Call this from every path that changes @verification_status@, @doc_type@ or
-- @pan_aadhaar_linkage@ on the person's PAN card. Missing one leaves a stale
-- rate that nothing else will ever correct.
materializeTdsRateFor :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Person.Person -> m ()
materializeTdsRateFor person = do
  transporterConfig <- getTransporterConfigFor person.merchantOperatingCityId
  when (Wallet.panAadhaarLinkTdsEnabled transporterConfig.taxConfig) $ do
    mbRate <- cohortRateFor transporterConfig person.id
    whenJust mbRate $ \rate ->
      setTdsRateValidatedWith transporterConfig.taxConfig person.id (isFleetOwnerRole person.role) (Just rate)

-- | The cohort's answer for this person, ignoring whatever is currently stored.
cohortRateFor :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DTC.TransporterConfig -> Id Person.Person -> m (Maybe Double)
cohortRateFor transporterConfig personId = do
  mbPanCard <- QPanCard.findByDriverId personId
  pure $ Wallet.computeEffectiveTdsRate mbPanCard Nothing transporterConfig.taxConfig

-- | Resolve the rate to charge, materialising it first if the column is empty.
--
-- Cohort enabled: the column is authoritative. An empty column is filled from
-- the cohort now, so the rate a ride charges and the rate stored against the
-- person can never disagree.
--
-- Cohort disabled: unchanged legacy behaviour -- backfill @defaultTdsRate@ once.
--
-- An out-of-range stored rate yields 'Nothing' rather than an exception: failing
-- the ride helps nobody. 'Nothing' means "ignore the column", not "no TDS" --
-- computeEffectiveTdsRate then resolves the cohort rate, or defaultTdsRate when
-- the cohort is off. So a corrupt column degrades to the correct rate rather
-- than to a skipped deduction.
ensureTdsRateFor ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DTC.TransporterConfig ->
  Id Person.Person ->
  -- | is this person a fleet owner (rate lives on fleet_owner_information)
  Bool ->
  -- | currently stored rate
  Maybe Double ->
  m (Maybe Double)
ensureTdsRateFor transporterConfig personId isFleet currentRate =
  case currentRate of
    Just rate
      | isValidTdsRate transporterConfig.taxConfig rate -> pure (Just rate)
      | otherwise -> do
        logError $
          "Stored TDS rate out of range for person " <> personId.getId
            <> ": "
            <> show rate
            <> " -- ignoring the stored value; the cohort (or defaultTdsRate) will supply the rate."
            <> " Clear the column so it can be re-materialised."
        pure Nothing
    Nothing -> do
      mbRate <-
        if Wallet.panAadhaarLinkTdsEnabled transporterConfig.taxConfig
          then cohortRateFor transporterConfig personId
          else pure ((.rate) <$> transporterConfig.taxConfig.defaultTdsRate)
      case mbRate of
        -- The cohort/default rate itself is out of range, i.e. the merchant's
        -- tax_config is internally inconsistent (a branch rate above
        -- invalidPanTdsRate). Skip the write so the bad value is not frozen into
        -- the column. This cannot stop the deduction: the caller resolves through
        -- computeEffectiveTdsRate, which reads the same config and will return the
        -- same rate. Only fixing tax_config fixes that.
        Just rate | not (isValidTdsRate transporterConfig.taxConfig rate) -> do
          logError $
            "Resolved TDS rate out of range for person " <> personId.getId
              <> ": "
              <> show rate
              <> " exceeds invalidPanTdsRate "
              <> show (maxAllowedTdsRate transporterConfig.taxConfig)
              <> " -- not storing it. The ride will still price off tax_config; fix the config."
          pure Nothing
        _ -> do
          whenJust mbRate $ \rate -> setTdsRateValidatedWith transporterConfig.taxConfig personId isFleet (Just rate)
          pure mbRate
