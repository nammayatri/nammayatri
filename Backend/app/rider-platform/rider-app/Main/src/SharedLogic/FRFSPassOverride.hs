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
    remainingTrips,
    benefitFromPass,
    isUnlimitedBenefit,
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

isFullyPassCovered :: Maybe HighPrecMoney -> Bool
isFullyPassCovered = maybe False (<= 0)

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

data PassCandidate = PassCandidate
  { payment :: DPPP.PurchasedPassPayment,
    pass :: DPass.Pass,
    benefit :: OverrideBenefit,
    availableTripCount :: Maybe Int
  }

loadPassCandidates ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DP.Person ->
  Maybe Bool ->
  T.Day ->
  m [PassCandidate]
loadPassCandidates person mbKnownHasPass tripDay = do
  hasPass <- maybe (checkHasPass person tripDay) pure mbKnownHasPass
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

toCandidate :: (CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> m (Maybe PassCandidate)
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
              availableTripCount <- remainingTrips payment benefit
              pure . Just $ PassCandidate {payment = payment, pass = pass, benefit = benefit, availableTripCount = availableTripCount}

remainingTrips :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> OverrideBenefit -> m (Maybe Int)
remainingTrips payment benefit
  | isUnlimitedBenefit benefit = pure Nothing
  | otherwise = Just <$> seededRemainingTrips payment (allowanceFor payment benefit)

allowanceFor :: DPPP.PurchasedPassPayment -> OverrideBenefit -> Int
allowanceFor payment benefit = fromMaybe (fromMaybe 0 benefit.maximumTripCount) payment.availableTripCount

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
toApplicablePass vehicleType tripDay payment = do
  mbCandidate <- toCandidate payment
  pure $ case mbCandidate of
    Nothing -> Nothing
    Just candidate -> listToMaybe (filterCandidatesForLeg [candidate] vehicleType tripDay)

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
    AlreadyConsumed
  deriving (Show, Eq)

makeTripCountKey :: Id DPPP.PurchasedPassPayment -> Text
makeTripCountKey paymentId = "availableTripCount:" <> paymentId.getId

tripCountTtl :: DPPP.PurchasedPassPayment -> UTCTime -> Int
tripCountTtl payment now =
  let secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 payment.endDate) 0) now
   in secondsTillEnd + (7 * 24 * 60 * 60)

seededRemainingTrips :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> Int -> m Int
seededRemainingTrips payment allowance = do
  let key = makeTripCountKey payment.id
  Redis.safeGet key >>= \case
    Just remaining -> pure remaining
    Nothing -> do
      now <- getCurrentTime
      seeded <- Redis.setNxExpire key (tripCountTtl payment now) allowance
      if seeded
        then do
          logInfo $ "FRFSPassOverride: seeded tripCount paymentId=" <> payment.id.getId <> " allowance=" <> show allowance
          pure allowance
        else fromMaybe allowance <$> Redis.safeGet key

refreshTripCountTtl :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> Text -> m ()
refreshTripCountTtl payment key = do
  now <- getCurrentTime
  Redis.expire key (tripCountTtl payment now)

claimTripMarker :: (MonadFlow m, Redis.HedisFlow m r) => Text -> Int -> Id DFRFSSearch.FRFSSearch -> m Bool
claimTripMarker phase ttl searchId =
  Redis.setNxExpire ("FRFSPassOverride:" <> phase <> "-" <> searchId.getId) ttl True

passMarkerTtl :: Int
passMarkerTtl = 120

consumeTripOnce :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DFRFSSearch.FRFSSearch -> DPPP.PurchasedPassPayment -> OverrideBenefit -> m ConsumeResult
consumeTripOnce searchId payment benefit = do
  firstConsume <- claimTripMarker "TripConsumed" passMarkerTtl searchId
  if not firstConsume
    then do
      logWarning $ "FRFSPassOverride:consumeTripOnce duplicate booking for searchId=" <> searchId.getId <> ", not applying override"
      pure AlreadyConsumed
    else consumeTrip payment benefit

consumeTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> OverrideBenefit -> m ConsumeResult
consumeTrip payment benefit
  | isUnlimitedBenefit benefit = pure Unmetered
  | otherwise = do
    let allowance = allowanceFor payment benefit
    void $ seededRemainingTrips payment allowance
    let key = makeTripCountKey payment.id
    firstTry <- Redis.decrIfExist key
    remaining <-
      if firstTry < 0
        then do
          logWarning $ "FRFSPassOverride:consumeTrip key missing or zero, reseeding paymentId=" <> payment.id.getId
          void $ seededRemainingTrips payment allowance
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

refundTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> OverrideBenefit -> m ()
refundTrip payment benefit = unless (isUnlimitedBenefit benefit) $ do
  let key = makeTripCountKey payment.id
  void $ seededRemainingTrips payment (allowanceFor payment benefit)
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
  QPersonExtra.setHasPassTillIfUnset personId endDate
  QPersonExtra.updateHasPassTill personId endDate

makeHasPassKey :: Id DP.Person -> Text
makeHasPassKey personId = "FRFSPassOverride:HasPass-" <> personId.getId

hasPassTtlBuffer :: Int
hasPassTtlBuffer = 7 * 24 * 60 * 60

checkHasPass :: (CacheFlow m r) => DP.Person -> T.Day -> m Bool
checkHasPass person today =
  Redis.safeGet (makeHasPassKey person.id) >>= \case
    Just True -> pure True
    _ -> case person.hasPassTill of
      Just till | till >= today -> do
        reseatHasPass person.id till
        pure True
      _ -> pure False

reseatHasPass :: (CacheFlow m r) => Id DP.Person -> T.Day -> m ()
reseatHasPass personId endDate = do
  now <- getCurrentTime
  let secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 endDate) 0) now
  Redis.setExp (makeHasPassKey personId) True (secondsTillEnd + hasPassTtlBuffer)

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
    Just payment -> do
      mbBenefit <- maybe (pure Nothing) (\passId -> CQPass.findById passId >>= maybe (pure Nothing) benefitFromPass) payment.passId
      case mbBenefit of
        Nothing -> logWarning $ "FRFSPassOverride:refundPassOverrideTrip no benefit config, nothing to give back paymentId=" <> paymentId.getId
        Just benefit -> do
          firstRelease <- claimTripMarker "TripReleased" passMarkerTtl searchId
          if firstRelease
            then refundTrip payment benefit
            else logInfo $ "FRFSPassOverride:refundPassOverrideTrip already released for searchId=" <> searchId.getId

releasePassOverrideTripOnFailure :: (CacheFlow m r, EsqDBFlow m r) => Id DFRFSSearch.FRFSSearch -> Maybe Text -> m ()
releasePassOverrideTripOnFailure searchId mbEntityId =
  whenJust mbEntityId $ \entityId -> do
    logInfo $ "FRFSPassOverride: releasing trip for failed booking searchId=" <> searchId.getId <> " paymentId=" <> entityId
    refundPassOverrideTrip searchId (Id entityId)
