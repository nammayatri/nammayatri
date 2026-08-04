module SharedLogic.FRFSPassOverride
  ( OverrideBenefitConfig (..),
    OverrideBenefit (..),
    PercentageSaving (..),
    FixedSaving (..),
    ApplicablePass (..),
    PassOption (..),
    mkPassOptionAPIEntity,
    passOptionsForQuote,
    resolvePassOverride,
    refundPassOverrideTrip,
    releasePassOverrideTripOnFailure,
    maxTripCountFromPass,
    isUnlimitedPass,
    registerHasPass,
    checkHasPass,
    getFRFSOverrideApplicablePassesByPersonId,
    applyOverrideBenefit,
    benefitForOverrideAppliedEntity,
    parseOverrideBenefitConfig,
    isFullyPassCovered,
    PassCandidate (..),
    loadPassCandidates,
    filterCandidatesForLeg,
    localTripDay,
    withinQuantityCap,
    ConsumeResult (..),
    getRemainingTripCount,
    consumeTrip,
    consumeTripOnce,
    refundTrip,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Data.Time as T
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (constructorsWithSnakeCase)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.Pass as CQPass
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment

newtype OverrideBenefitConfig = OverrideBenefitConfig
  { overrideBenefits :: [OverrideBenefit]
  }
  deriving (Generic, Show)

data OverrideBenefit = OverrideBenefit
  { percentageSaving :: Maybe PercentageSaving,
    fixedSaving :: Maybe FixedSaving,
    unlimitedTripCount :: Maybe Bool,
    maximumTripCount :: Maybe Int,
    maxTicketQuantityPerOverride :: Maybe Int
  }
  deriving (Generic, Show)

data PercentageSaving = PercentageSaving
  { applicableValue :: HighPrecMoney,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show)

data FixedSaving = FixedSaving
  { applicableValue :: HighPrecMoney,
    currencyType :: Maybe Currency,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON OverrideBenefitConfig where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefitConfig where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON OverrideBenefit where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefit where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON PercentageSaving where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON PercentageSaving where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON FixedSaving where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON FixedSaving where
  toJSON = A.genericToJSON constructorsWithSnakeCase

-- @<= 0@ rather than @== 0@: a benefit that overshoots the fare must not read as
-- "partially covered", which would send the booking down the payment path with nothing to collect.
isFullyPassCovered :: Maybe HighPrecMoney -> Bool
isFullyPassCovered = maybe False (<= 0)

-- The cap is the whole basket, not per category, and an absent cap means one ticket.
-- Both the offer side (passOptionsForQuote) and the multimodal pre-flight must agree on this
-- or a leg is offered a pass that its own confirm then rejects.
withinQuantityCap :: Int -> Maybe Int -> Bool
withinQuantityCap totalQuantity mbCap = totalQuantity <= fromMaybe 1 mbCap

data ApplicablePass = ApplicablePass
  { purchasedPassPayment :: DPPP.PurchasedPassPayment,
    pass :: DPass.Pass,
    benefit :: OverrideBenefit,
    availableTripCount :: Maybe Int,
    unlimitedTripCount :: Bool
  }

parseOverrideBenefitConfig :: A.Value -> Either String OverrideBenefitConfig
parseOverrideBenefitConfig value = case A.fromJSON value of
  A.Success config -> Right config
  A.Error err -> Left err

coverageSupported :: DIBC.IntegratedBPPConfig -> Price -> Bool
coverageSupported integratedBPPConfig overriddenTotal =
  not (isOndcConfig integratedBPPConfig && isFullyPassCovered (Just overriddenTotal.amount))

isOndcConfig :: DIBC.IntegratedBPPConfig -> Bool
isOndcConfig integratedBPPConfig = case integratedBPPConfig.providerConfig of
  DIBC.ONDC _ -> True
  _ -> False

-- Checked explicitly because aeson ignores unknown keys: a misspelled field reads as absent and
-- silently changes what the rider is charged. See .cursor/docs/20-frfs-pass-fare-override.md
validateBenefit :: OverrideBenefit -> Either Text OverrideBenefit
validateBenefit benefit
  | not (savingEnabled benefit.percentageSaving (.enabled)) && not (savingEnabled benefit.fixedSaving (.enabled)) =
    Left "no enabled saving: exactly one of percentage_saving or fixed_saving must have enabled=true"
  | savingEnabled benefit.percentageSaving (.enabled) && savingEnabled benefit.fixedSaving (.enabled) =
    Left "both percentage_saving and fixed_saving are enabled; only one may be"
  | Just p <- benefit.percentageSaving,
    p.enabled == Just True,
    p.applicableValue <= 0 || p.applicableValue > 100 =
    Left $ "percentage_saving.applicable_value must be in (0, 100], got " <> show p.applicableValue
  | Just f <- benefit.fixedSaving,
    f.enabled == Just True,
    f.applicableValue <= 0 =
    Left $ "fixed_saving.applicable_value must be positive, got " <> show f.applicableValue
  | not (isUnlimitedBenefit benefit) && isNothing benefit.maximumTripCount =
    Left "benefit is metered but has no maximum_trip_count"
  | maybe False (<= 0) benefit.maximumTripCount =
    Left "maximum_trip_count must be positive"
  | maybe False (<= 0) benefit.maxTicketQuantityPerOverride =
    Left "max_ticket_quantity_per_override must be positive"
  | otherwise = Right benefit
  where
    savingEnabled mbSaving getEnabled = maybe False ((== Just True) . getEnabled) mbSaving

benefitFromPass :: (Log m, MonadFlow m) => DPass.Pass -> m (Maybe OverrideBenefit)
benefitFromPass pass = case pass.overrideBenefitConfigJson of
  Nothing -> do
    logError $ "FRFSPassOverride: pass is override-applicable but has no benefit config passId=" <> pass.id.getId
    pure Nothing
  Just configJson -> case parseOverrideBenefitConfig configJson of
    Left err -> do
      logError $ "FRFSPassOverride: unparseable benefit config passId=" <> pass.id.getId <> " error=" <> show err
      pure Nothing
    Right config -> do
      -- Only the first entry is ever applied. Saying so out loud because the schema is a list and
      -- an author could reasonably expect several benefits to compose.
      when (length config.overrideBenefits > 1) $
        logError $ "FRFSPassOverride: override_benefits has " <> show (length config.overrideBenefits) <> " entries, only the first is applied passId=" <> pass.id.getId
      case listToMaybe config.overrideBenefits of
        Nothing -> do
          logError $ "FRFSPassOverride: empty override_benefits passId=" <> pass.id.getId
          pure Nothing
        Just benefit -> case validateBenefit benefit of
          Left reason -> do
            logError $ "FRFSPassOverride: invalid benefit config, disqualifying pass passId=" <> pass.id.getId <> " reason=" <> reason
            pure Nothing
          Right valid -> pure (Just valid)

isUnlimitedBenefit :: OverrideBenefit -> Bool
isUnlimitedBenefit benefit = benefit.unlimitedTripCount == Just True

-- Everything about a rider's passes that does not depend on which leg is being priced: the payments
-- scan, the pass rows, their benefit configs and their remaining trip counts. Resolved once per
-- journey and filtered per leg by filterCandidatesForLeg, because a journey would otherwise repeat
-- all of it for every FRFS leg to compute filters over identical data.
data PassCandidate = PassCandidate
  { payment :: DPPP.PurchasedPassPayment,
    pass :: DPass.Pass,
    benefit :: OverrideBenefit,
    availableTripCount :: Maybe Int
  }

-- No IntegratedBPPConfig here on purpose. passOverrideApplicable is a per-config value and a
-- journey can span operators, so gating the shared load on any one of them would blank the passes
-- for legs served by another. Callers gate with their own leg's config -- see
-- mkLegInfoFromFrfsSearchRequest -- and this only answers "which passes does the rider hold".
loadPassCandidates ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DP.Person ->
  Maybe Bool ->
  T.Day ->
  m [PassCandidate]
loadPassCandidates person mbClientHasPasses tripDay = do
  hasPass <- if mbClientHasPasses == Just True then pure True else checkHasPass person tripDay
  if not hasPass
    then pure []
    else do
      payments <-
        QPurchasedPassPayment.findAllByPersonIdAndStatuses
          Nothing
          Nothing
          person.id
          [DPurchasedPass.Active, DPurchasedPass.PreBooked]
      catMaybes <$> mapM toCandidate payments
  where
    toCandidate payment = case payment.passId of
      Nothing -> pure Nothing
      Just passId ->
        CQPass.findById passId >>= \case
          Nothing -> pure Nothing
          Just pass
            | pass.frfsPriceOverrideApplicable /= Just True -> pure Nothing
            | otherwise ->
              benefitFromPass pass >>= \case
                Nothing -> pure Nothing
                Just benefit -> do
                  availableTripCount <- getRemainingTripCount payment
                  pure . Just $ PassCandidate {payment = payment, pass = pass, benefit = benefit, availableTripCount = availableTripCount}

-- Pure: the only per-leg inputs are the leg's vehicle category and its departure day.
filterCandidatesForLeg :: [PassCandidate] -> Spec.VehicleCategory -> T.Day -> [ApplicablePass]
filterCandidatesForLeg candidates vehicleType tripDay =
  [ ApplicablePass
      { purchasedPassPayment = candidate.payment,
        pass = candidate.pass,
        benefit = candidate.benefit,
        availableTripCount = candidate.availableTripCount,
        unlimitedTripCount = isUnlimitedBenefit candidate.benefit
      }
    | candidate <- candidates,
      candidate.pass.vehicleType == vehicleType,
      candidate.payment.startDate <= tripDay,
      candidate.payment.endDate >= tripDay,
      not (maybe False (<= 0) candidate.availableTripCount)
  ]

getFRFSOverrideApplicablePassesByPersonId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DIBC.IntegratedBPPConfig ->
  DP.Person ->
  Spec.VehicleCategory ->
  UTCTime ->
  Maybe Bool ->
  m [ApplicablePass]
getFRFSOverrideApplicablePassesByPersonId integratedBPPConfig person vehicleType tripTime mbClientHasPasses
  | integratedBPPConfig.passOverrideApplicable /= Just True = pure []
  | otherwise = do
    tripDay <- localTripDay person tripTime
    candidates <- loadPassCandidates person mbClientHasPasses tripDay
    pure $ filterCandidatesForLeg candidates vehicleType tripDay

-- startDate/endDate are written from the local calendar day at purchase (getLocalCurrentTime
-- timeDiffFromUtc), so validity has to be compared on the same calendar. Taking utctDay here
-- makes 00:00-05:30 IST belong to the previous day: a pass starting today is rejected, and one
-- that expired yesterday still grants a free trip.
localTripDay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> UTCTime -> m T.Day
localTripDay person tripTime = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  pure . T.utctDay $ addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) tripTime

toApplicablePass ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Spec.VehicleCategory ->
  T.Day ->
  DPPP.PurchasedPassPayment ->
  m (Maybe ApplicablePass)
toApplicablePass vehicleType tripDay payment
  | not withinValidity = pure Nothing
  | otherwise = case payment.passId of
    Nothing -> pure Nothing
    Just passId ->
      CQPass.findById passId >>= \case
        Nothing -> pure Nothing
        Just pass
          | not (isOverridePass pass) -> pure Nothing
          | otherwise ->
            benefitFromPass pass >>= \case
              Nothing -> pure Nothing
              Just benefit -> do
                availableTripCount <- getRemainingTripCount payment
                if maybe False (<= 0) availableTripCount
                  then pure Nothing
                  else pure . Just $ ApplicablePass {purchasedPassPayment = payment, pass = pass, benefit = benefit, availableTripCount = availableTripCount, unlimitedTripCount = isUnlimitedBenefit benefit}
  where
    withinValidity = payment.startDate <= tripDay && payment.endDate >= tripDay
    isOverridePass pass =
      pass.vehicleType == vehicleType
        && pass.frfsPriceOverrideApplicable == Just True

isUnlimitedPass :: (Log m, MonadFlow m) => DPass.Pass -> m Bool
isUnlimitedPass pass
  | pass.frfsPriceOverrideApplicable /= Just True = pure False
  | otherwise = maybe False isUnlimitedBenefit <$> benefitFromPass pass

maxTripCountFromPass :: (Log m, MonadFlow m) => DPass.Pass -> m (Maybe Int)
maxTripCountFromPass pass
  | pass.frfsPriceOverrideApplicable /= Just True = pure Nothing
  | otherwise =
    benefitFromPass pass <&> \mbBenefit -> do
      benefit <- mbBenefit
      guard (not (isUnlimitedBenefit benefit))
      benefit.maximumTripCount

data PassOption = PassOption
  { purchasedPassPaymentId :: Id DPPP.PurchasedPassPayment,
    passId :: Id DPass.Pass,
    passName :: Maybe Text,
    overriddenUnitPrice :: Price,
    overriddenTotalPrice :: Price,
    availableTripCount :: Maybe Int,
    unlimitedTripCount :: Bool,
    maxTicketQuantityPerOverride :: Maybe Int
  }
  deriving (Show, Generic)

-- The benefit is per ticket, so the basket total is the per-item override summed back up, never
-- the benefit applied once to the sum. For a percentage the two agree; for a fixed saving they
-- differ by (quantity - 1) times the saving.
passOptionsForQuote :: DIBC.IntegratedBPPConfig -> [ApplicablePass] -> Maybe Spec.ServiceTierType -> Price -> [(Price, Int)] -> [PassOption]
passOptionsForQuote integratedBPPConfig applicablePasses mbServiceTier adultUnitPrice priceItems =
  [ PassOption
      { purchasedPassPaymentId = applicablePass.purchasedPassPayment.id,
        passId = applicablePass.pass.id,
        passName = applicablePass.purchasedPassPayment.passName,
        overriddenUnitPrice = applyOverrideBenefit applicablePass.benefit adultUnitPrice,
        overriddenTotalPrice = overriddenTotal,
        availableTripCount = applicablePass.availableTripCount,
        unlimitedTripCount = applicablePass.unlimitedTripCount,
        maxTicketQuantityPerOverride = applicablePass.benefit.maxTicketQuantityPerOverride
      }
    | applicablePass <- applicablePasses,
      coversTier applicablePass,
      coversQuantity applicablePass,
      let overriddenTotal = totalWith (applyOverrideBenefit applicablePass.benefit),
      overriddenTotal.amount < baseTotal.amount,
      coverageSupported integratedBPPConfig overriddenTotal
  ]
  where
    totalQuantity = sum (map snd priceItems)
    totalWith :: (Price -> Price) -> Price
    totalWith f =
      mkPrice (Just adultUnitPrice.currency) . HighPrecMoney . sum $
        [(f unitPrice).amount.getHighPrecMoney * toRational quantity | (unitPrice, quantity) <- priceItems]
    -- Not `id`: Kernel.Prelude does not export it and Kernel.Types.Id's IdObject selector
    -- shadows the name, so `id` here resolves to `IdObject -> Text`. Do not let --apply-hint
    -- "simplify" this lambda; it will not compile.
    baseTotal = totalWith (\unitPrice -> unitPrice)
    coversTier applicablePass =
      maybe False (`elem` applicablePass.pass.applicableVehicleServiceTiers) mbServiceTier
    coversQuantity applicablePass =
      withinQuantityCap totalQuantity applicablePass.benefit.maxTicketQuantityPerOverride

applyOverrideBenefit :: OverrideBenefit -> Price -> Price
applyOverrideBenefit benefit basePrice
  | Just percentage <- benefit.percentageSaving,
    isEnabled percentage.enabled,
    percentage.applicableValue > 0 =
    discounted $ basePrice.amount * percentage.applicableValue / 100
  | Just fixed <- benefit.fixedSaving,
    isEnabled fixed.enabled,
    fixed.applicableValue > 0 =
    discounted fixed.applicableValue
  | otherwise = basePrice
  where
    isEnabled = fromMaybe False
    discounted saving = mkPrice (Just basePrice.currency) (max 0 (basePrice.amount - saving))

-- Re-derives the benefit: only the booking-level overriddenAmount is persisted. Callers must
-- reconcile the recomputed total against it -- see createBasketFromBookings and .cursor/docs/20-frfs-pass-fare-override.md
benefitForOverrideAppliedEntity :: (CacheFlow m r, EsqDBFlow m r) => Maybe Text -> m (Maybe OverrideBenefit)
benefitForOverrideAppliedEntity Nothing = pure Nothing
benefitForOverrideAppliedEntity (Just entityId) = do
  mbPayment <- QPurchasedPassPayment.findByPrimaryKey (Id entityId)
  case mbPayment >>= (.passId) of
    Nothing -> pure Nothing
    Just passId ->
      CQPass.findById passId >>= \case
        Nothing -> pure Nothing
        Just pass -> benefitFromPass pass

data ConsumeResult
  = Consumed Int
  | Unmetered
  | Exhausted
  | -- | Another booking for this same leg search already spent the trip. The caller must not stamp
    -- override fields for this one: leaving it at full fare sends it down the payment path, where
    -- it strands at NEW and expires like any other duplicate booking. That is deliberate -- a fully
    -- covered booking skips payment entirely, so without this a duplicate would confirm with the
    -- operator and the rider would get a second real ticket they never asked for.
    AlreadyConsumed
  deriving (Show, Eq)

makeTripCountKey :: Id DPPP.PurchasedPassPayment -> Text
makeTripCountKey paymentId = "availableTripCount:" <> paymentId.getId

tripCountTtl :: DPPP.PurchasedPassPayment -> UTCTime -> Int
tripCountTtl payment now =
  let secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 payment.endDate) 0) now
   in secondsTillEnd + (7 * 24 * 60 * 60)

getRemainingTripCount :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> m (Maybe Int)
-- available_trip_count mirrors the remainder, so it is what a cold Redis key re-seeds from.
getRemainingTripCount payment = case payment.availableTripCount of
  Nothing -> pure Nothing
  Just allowance -> do
    let key = makeTripCountKey payment.id
    Redis.safeGet key >>= \case
      Just remaining -> pure (Just remaining)
      Nothing -> do
        now <- getCurrentTime
        seeded <- Redis.setNxExpire key (tripCountTtl payment now) allowance
        if seeded
          then do
            logInfo $ "FRFSPassOverride: seeded tripCount paymentId=" <> payment.id.getId <> " allowance=" <> show allowance
            pure (Just allowance)
          else Just . fromMaybe allowance <$> Redis.safeGet key

refreshTripCountTtl :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> Text -> m ()
refreshTripCountTtl payment key = do
  now <- getCurrentTime
  Redis.expire key (tripCountTtl payment now)

-- Both sides of the counter need "do this at most once per leg", so the SETNX dance lives here
-- rather than being open-coded at each call site. Returns True if the caller won the claim.
-- Deliberately non-blocking: a loser skips its effect, it does not wait or fail.
--
-- Keyed on the leg search, not the booking: the race produces two different booking ids, so a
-- booking-keyed marker would not collide. See .cursor/docs/20-frfs-pass-fare-override.md
claimTripMarker :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Int -> Id DFRFSSearch.FRFSSearch -> m Bool
claimTripMarker phase ttl searchId =
  Redis.setNxExpire ("FRFSPassOverride:" <> phase <> "-" <> searchId.getId) ttl True

-- See passMarkerTtl for why this is short.
-- Both markers guard the same window: another attempt arriving while the first is still in flight.
-- Anything later is already deduplicated without them -- a repeat confirm finds the existing booking
-- via findBySearchId and never reaches a consume, and a late release is locked out by the booking's
-- terminal status, which every release path sets and the FRFSStatus expiry check skips. The proxy
-- cuts clients at 40s, so two minutes is ample for both.
passMarkerTtl :: Int
passMarkerTtl = 120

-- Idempotent consume: at most one trip per leg search. The marker is claimed BEFORE metering is
-- considered -- unlimited passes have availableTripCount = Nothing and would otherwise be
-- unguarded entirely. See .cursor/docs/20-frfs-pass-fare-override.md
consumeTripOnce :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DFRFSSearch.FRFSSearch -> DPPP.PurchasedPassPayment -> m ConsumeResult
consumeTripOnce searchId payment = do
  firstConsume <- claimTripMarker "TripConsumed" passMarkerTtl searchId
  if not firstConsume
    then do
      logWarning $ "FRFSPassOverride:consumeTripOnce duplicate booking for searchId=" <> searchId.getId <> ", not applying override"
      pure AlreadyConsumed
    else case payment.availableTripCount of
      Nothing -> pure Unmetered
      Just _ -> consumeTrip payment

-- decrIfExist rather than a bare DECR, and that is the whole point: it decrements only when the key
-- exists and holds a positive value, and returns -1 otherwise -- it never *creates* the key. A bare
-- DECR on a key evicted between the seed and the decrement creates it at -1, which reads as
-- Exhausted, gets INCRed back to 0, and since Redis is preferred over the column the pass then
-- reads as spent for the rest of the key's TTL (pass lifetime plus seven days).
--
-- -1 is ambiguous between "evicted" and "genuinely zero", so a single reseed-and-retry separates
-- them: after seeding from the mirrored remainder, a second -1 means the rider really is out.
consumeTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> m ConsumeResult
consumeTrip payment = case payment.availableTripCount of
  Nothing -> pure Unmetered
  Just _ -> do
    void $ getRemainingTripCount payment
    let key = makeTripCountKey payment.id
    firstTry <- Redis.decrIfExist key
    remaining <-
      if firstTry < 0
        then do
          logWarning $ "FRFSPassOverride:consumeTrip key missing or zero, reseeding paymentId=" <> payment.id.getId
          void $ getRemainingTripCount payment
          Redis.decrIfExist key
        else pure firstTry
    if remaining < 0
      then do
        logWarning $ "FRFSPassOverride:consumeTrip EXHAUSTED paymentId=" <> payment.id.getId
        pure Exhausted
      else do
        refreshTripCountTtl payment key
        QPurchasedPassPayment.updateAvailableTripCountById (Just (fromIntegral remaining)) payment.id
        pure $ Consumed (fromIntegral remaining)

-- No ceiling check needed: releases can never exceed consumes. Seeds first so an absent key
-- resumes from the mirrored remainder rather than being created at 1. See .cursor/docs/20-frfs-pass-fare-override.md
refundTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> m ()
refundTrip payment = when (isJust payment.availableTripCount) $ do
  let key = makeTripCountKey payment.id
  -- Seeds first so an absent key resumes from the mirrored remainder rather than being created at 1
  -- by the incr. INCR is safe to use directly here: unlike the decrement there is no negative value
  -- it could wrongly produce, and the release marker already caps this at one call per leg search.
  void $ getRemainingTripCount payment
  remaining <- Redis.incr key
  refreshTripCountTtl payment key
  QPurchasedPassPayment.updateAvailableTripCountById (Just (fromIntegral remaining)) payment.id
  logInfo $ "FRFSPassOverride:refundTrip paymentId=" <> payment.id.getId <> " -> " <> show remaining

registerHasPass :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> T.Day -> m ()
registerHasPass personId endDate = do
  now <- getCurrentTime
  let key = makeHasPassKey personId
      secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 endDate) 0) now
      newTtl = secondsTillEnd + hasPassTtlBuffer
  existingTtl <- fromInteger <$> Redis.ttl key
  Redis.setExp key True (max newTtl existingTtl)
  mbPerson <- QPerson.findById personId
  let currentTill = mbPerson >>= (.hasPassTill)
  when (maybe True (< endDate) currentTill) $ QPersonExtra.updateHasPassTill personId endDate

makeHasPassKey :: Id DP.Person -> Text
makeHasPassKey personId = "FRFSPassOverride:HasPass-" <> personId.getId

hasPassTtlBuffer :: Int
hasPassTtlBuffer = 7 * 24 * 60 * 60

-- Column first (the Person is already loaded), Redis only when it says no -- registerHasPass is
-- wrapped in withTryCatch at purchase, so the key is the second chance if that write is lost.
checkHasPass :: (CacheFlow m r) => DP.Person -> T.Day -> m Bool
checkHasPass person today
  | maybe False (>= today) person.hasPassTill = pure True
  | otherwise = fromMaybe False <$> Redis.safeGet (makeHasPassKey person.id)

mkPassOptionAPIEntity :: PassOption -> FRFSTicketServiceAPI.FRFSPassOptionAPIEntity
mkPassOptionAPIEntity passOption =
  FRFSTicketServiceAPI.FRFSPassOptionAPIEntity
    { purchasedPassPaymentId = passOption.purchasedPassPaymentId,
      passId = passOption.passId,
      passName = passOption.passName,
      overriddenUnitPrice = mkPriceAPIEntity passOption.overriddenUnitPrice,
      overriddenTotalPrice = mkPriceAPIEntity passOption.overriddenTotalPrice,
      availableTripCount = passOption.availableTripCount,
      unlimitedTripCount = passOption.unlimitedTripCount,
      maxTicketQuantityPerOverride = passOption.maxTicketQuantityPerOverride
    }

resolvePassOverride ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DIBC.IntegratedBPPConfig ->
  DP.Person ->
  Spec.VehicleCategory ->
  UTCTime ->
  Maybe Spec.ServiceTierType ->
  Price ->
  [(Price, Int)] ->
  Id DPPP.PurchasedPassPayment ->
  m (Maybe (ApplicablePass, PassOption))
resolvePassOverride integratedBPPConfig person vehicleType tripTime mbServiceTier adultUnitPrice priceItems paymentId
  | integratedBPPConfig.passOverrideApplicable /= Just True = pure Nothing
  | otherwise = do
    tripDay <- localTripDay person tripTime
    mbPayment <- QPurchasedPassPayment.findByPrimaryKey paymentId
    -- Fetched by id rather than scanning the rider's payments, so the ownership check that the
    -- person-scoped query used to give implicitly has to be made explicit here. Without it the
    -- client could spend someone else's pass by id.
    mbApplicablePass <- case mbPayment of
      Just payment
        | payment.personId == person.id
            && payment.status `elem` [DPurchasedPass.Active, DPurchasedPass.PreBooked] ->
          toApplicablePass vehicleType tripDay payment
      _ -> pure Nothing
    case mbApplicablePass of
      Nothing -> do
        logWarning $ "FRFSPassOverride: pass not applicable paymentId=" <> paymentId.getId
        pure Nothing
      Just applicablePass ->
        case passOptionsForQuote integratedBPPConfig [applicablePass] mbServiceTier adultUnitPrice priceItems of
          [] -> do
            logWarning $
              "FRFSPassOverride: no applicable option for pass paymentId="
                <> paymentId.getId
                <> " (tier/quantity mismatch, no longer cheaper, or full coverage on an ONDC config) ondcConfig="
                <> show (isOndcConfig integratedBPPConfig)
            pure Nothing
          (passOption : _) -> pure (Just (applicablePass, passOption))

refundPassOverrideTrip :: (CacheFlow m r, EsqDBFlow m r) => Id DFRFSSearch.FRFSSearch -> Id DPPP.PurchasedPassPayment -> m ()
refundPassOverrideTrip searchId paymentId = do
  mbPayment <- QPurchasedPassPayment.findByPrimaryKey paymentId
  case mbPayment of
    Nothing -> logWarning $ "FRFSPassOverride:refundPassOverrideTrip payment not found paymentId=" <> paymentId.getId
    Just payment
      | isNothing payment.availableTripCount -> pure ()
      | otherwise -> do
        -- Short, like the consume side. The booking's terminal status is what actually deduplicates
        -- releases -- every path marks FAILED/CANCELLED before releasing, and the FRFSStatus expiry
        -- check explicitly skips those statuses -- so this marker only has to catch paths firing
        -- concurrently, not minutes apart. It used to be pass-lifetime + 7 days, which left one key
        -- per released booking lying around for weeks for no benefit.
        --
        -- The asymmetry (300s consume vs pass-lifetime release) is only safe because a search can
        -- never produce a second consume once the first has expired. findBySearchId reuses the
        -- existing booking and isMultiInitAllowed excludes FAILED, so buildAndCreateBooking is
        -- never re-entered for a search that already has one. If that ever changes -- if a FAILED
        -- booking becomes re-bookable on the same search -- a confirm hours later would find the
        -- consume marker expired, consume again, and then hit this already-set release marker on
        -- its own failure, stranding the trip. Widen the consume TTL if you make that possible.
        firstRelease <- claimTripMarker "TripReleased" passMarkerTtl searchId
        if firstRelease
          then refundTrip payment
          else logInfo $ "FRFSPassOverride:refundPassOverrideTrip already released for searchId=" <> searchId.getId

releasePassOverrideTripOnFailure :: (CacheFlow m r, EsqDBFlow m r) => Id DFRFSSearch.FRFSSearch -> Maybe Text -> m ()
releasePassOverrideTripOnFailure searchId mbEntityId =
  whenJust mbEntityId $ \entityId -> do
    logInfo $ "FRFSPassOverride: releasing trip for failed booking searchId=" <> searchId.getId <> " paymentId=" <> entityId
    refundPassOverrideTrip searchId (Id entityId)
