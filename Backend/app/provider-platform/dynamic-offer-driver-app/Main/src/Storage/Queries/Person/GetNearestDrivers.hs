module Storage.Queries.Person.GetNearestDrivers
  ( getNearestDrivers,
    fetchSortedLTSCandidates,
    processCandidatesChunk,
    buildDriverResult,
    isTierEligibleForDriver,
    scheduledTierEligibleForDriver,
    isDriverModeEligibleHelper,
    SortedLTSCandidate (..),
    NearestDriversResult (..),
    NearestDriversReq (..),
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverLocation (DriverLocation)
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import Domain.Types.Merchant
import Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant as DV
import Domain.Utils
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DriverPool.DriverPoolData as DPD
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.Prepaid
import SharedLogic.Finance.WalletAccount
import Storage.Beam.Finance ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverLocation.Internal as Int
import Tools.Error

data NearestDriversResult = NearestDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.VehicleVariant,
    serviceTier :: ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    driverTags :: A.Value,
    selectedAutoAcceptTiers :: [ServiceTierType],
    score :: Maybe A.Value,
    tripDistanceMinThreshold :: Maybe Meters,
    tripDistanceMaxThreshold :: Maybe Meters,
    maxPickupDistance :: Maybe Meters,
    isPetModeEnabled :: Bool,
    isTollRouteEligible :: Bool, -- True if tollRouteBlockedTill is Nothing or < now
    driverGender :: Person.Gender,
    vehicleNumber :: Maybe Text,
    fleetOwnerId :: Maybe Text,
    -- On-ride forward batching fields (Nothing for non-on-ride drivers)
    previousRideDropLat :: Maybe Double,
    previousRideDropLon :: Maybe Double,
    distanceFromDriverToDestination :: Maybe Meters
  }
  deriving (Generic, Show, HasCoordinates)

data NearestDriversReq = NearestDriversReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    serviceTiers :: [ServiceTierType],
    fromLocLatLong :: LatLong,
    nearestRadius :: Meters,
    merchantId :: Id Merchant,
    driverPositionInfoExpiry :: Maybe Seconds,
    isRental :: Bool,
    isInterCity :: Bool,
    isScheduled :: Bool,
    scheduledOpenToAll :: Bool,
    currentRideTripCategoryValidForForwardBatching :: [Text],
    prepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    fleetPrepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    rideFare :: Maybe HighPrecMoney,
    -- | 'bufferedFare' per service tier -- the cap config lives on FarePolicy,
    -- which resolves per tier, so one tier's ceiling must never be applied to
    -- another tier's drivers.
    bufferedFareByTier :: Map.Map ServiceTierType HighPrecMoney,
    govtCharges :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    minWalletAmountForCashRides :: Maybe HighPrecMoney,
    minWalletAmountForScheduledRides :: Maybe HighPrecMoney,
    paymentInstrument :: Maybe MP.PaymentInstrument,
    taxConfig :: DTC.TaxConfig,
    driverWalletConfig :: DTC.DriverWalletConfig,
    mbSearchTryId :: Maybe Text,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    now :: UTCTime,
    paymentMode :: Maybe MP.PaymentMode,
    vehicleCategoryScopedPrepaidEnabled :: Bool,
    excludeDriverIds :: [Id Person.Driver],
    prevAttemptedDriverIds :: [Id Person.Driver],
    applyParallelRequestFilter :: Bool,
    maxParallelSearchRequests :: Int,
    airportEntryFee :: Maybe HighPrecMoney,
    isAirportRequest :: Bool
  }

-- | A driver location candidate sorted by straight-line distance, with the
-- previously-attempted flag preserved so that downstream chunking can keep
-- prev-attempted drivers at the tail (process them only when fresh drivers run out).
data SortedLTSCandidate = SortedLTSCandidate
  { driverLoc :: DriverLocation,
    straightLineDistanceMeters :: Double,
    isPrevAttempted :: Bool
  }
  deriving (Generic, Show)

-- | LTS fetch + exclude blocklisted + compute straight-line distance + sort.
-- Output: sorted candidates with NON-prev-attempted drivers first (by distance ASC),
-- then prev-attempted drivers (by distance ASC). This lets chunked callers process
-- fresh drivers first and only touch prev-attempted ones when fresh ones run out
-- (replaces the old fillBatch backfill mechanism).
fetchSortedLTSCandidates ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasShortDurationRetryCfg r c) =>
  NearestDriversReq ->
  m [SortedLTSCandidate]
fetchSortedLTSCandidates NearestDriversReq {..} = do
  let allowedCityServiceTiers = filter (\cvst -> cvst.serviceTierType `elem` serviceTiers) cityServiceTiers
      allowedVehicleVariant = DL.nub (concatMap (.allowedVehicleVariant) allowedCityServiceTiers)
  driverLocsRaw <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong nearestRadius (bool (Just allowedVehicleVariant) Nothing (null allowedVehicleVariant)) mbSearchTryId
  let afterExclude = if null excludeDriverIds then driverLocsRaw else filter (\dl -> dl.driverId `notElem` excludeDriverIds) driverLocsRaw
      prevSet = prevAttemptedDriverIds
      mkCandidate dl =
        let dist = (realToFrac $ distanceBetweenInMeters fromLocLatLong (LatLong dl.lat dl.lon)) :: Double
            isPrev = dl.driverId `elem` prevSet
         in SortedLTSCandidate dl dist isPrev
      withDist = map mkCandidate afterExclude
      (notPrev, prev) = DL.partition (not . isPrevAttempted) withDist
      sortedNotPrev = DL.sortOn straightLineDistanceMeters notPrev
      sortedPrev = DL.sortOn straightLineDistanceMeters prev
      sorted = sortedNotPrev <> sortedPrev
  logDebug $
    "DriverPool[1-LTS] " <> show (length sorted) <> " drivers within " <> show nearestRadius
      <> "m (excluded="
      <> show (length driverLocsRaw - length afterExclude)
      <> ", notPrev="
      <> show (length sortedNotPrev)
      <> ", prevAtTail="
      <> show (length sortedPrev)
      <> ")"
  pure sorted

-- | Process one chunk of sorted candidates: parallel-cap filter, pool-data fetch,
-- eligibility chain, service-tier expansion, wallet balance check.
-- Returns NearestDriversResult per (driver, matchingServiceTier) pair.
processCandidatesChunk ::
  (BeamFlow m r, MonadFlow m, MonadTime m, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  NearestDriversReq ->
  (Bool -> Bool -> [Id Person.Driver] -> m [DPD.DriverPoolData]) ->
  [SortedLTSCandidate] ->
  m [NearestDriversResult]
processCandidatesChunk req@NearestDriversReq {..} fetchPoolData chunk = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let isPrepaidEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  -- Parallel-cap filter (one Redis ZCOUNT per driver in the chunk).
  filteredChunk <-
    if applyParallelRequestFilter
      then filterM (parallelRequestsFilterForDriver req . (.driverId) . driverLoc) chunk
      else pure chunk
  -- Pool-data MGET for chunk survivors only.
  let chunkDriverIds = (.driverId) . driverLoc <$> filteredChunk
  poolDataList <- fetchPoolData onlinePayment isPrepaidEnabled chunkDriverIds
  let poolDataMap = HashMap.fromList $ (\dpd -> (dpd.driverId, dpd)) <$> poolDataList
      cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      results = concat $ mapMaybe (buildDriverResult req poolDataMap cityServiceTiersHashMap . driverLoc) filteredChunk
  filterByWalletBalance req isPrepaidEnabled results

-- | Wrapper for non-chunked callers (Estimate stage): fetch then process all as one chunk.
getNearestDrivers ::
  (BeamFlow m r, MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasShortDurationRetryCfg r c) =>
  NearestDriversReq ->
  (Bool -> Bool -> [Id Person.Driver] -> m [DPD.DriverPoolData]) ->
  m [NearestDriversResult]
getNearestDrivers req fetchPoolData = do
  candidates <- fetchSortedLTSCandidates req
  processCandidatesChunk req fetchPoolData candidates

-- | True if `tier` is either not cohort-gated at all, or the driver currently holds a valid,
-- unexpired tag for the cohort it's gated on. Also imported by SharedLogic.VehicleServiceTier's
-- candidate-selection/display logic, so this is the single shared definition -- defined here
-- (not there) to avoid a module cycle, since VehicleServiceTier needs Storage.Queries.Person for
-- an internal Person.driverTag fetch, and Storage.Queries.Person's submodule graph already routes
-- through this file. This is the layer that actually gates dispatch, so it must never trust
-- `selectedServiceTiers` alone for a cohort-gated tier. The cohort tag itself is always
-- ops-assigned (via the dashboard); no tier-selection change ever writes it.
--
-- The cohort tag's value is always "Cohort#<tier>" -- the same string as the tier itself, not a
-- separately configured short code -- so no Redis-backed short-code-to-tier mapping is needed
-- anywhere (Haskell or location-tracking-service) to answer "which tier does this cohort gate."
isTierEligibleForDriver :: UTCTime -> Maybe [LYT.TagNameValueExpiry] -> HashMap.HashMap ServiceTierType DVST.VehicleServiceTier -> ServiceTierType -> Bool
isTierEligibleForDriver now driverTag tierConfigs tier =
  case HashMap.lookup tier tierConfigs >>= (.availabilityCheckConfig) of
    Nothing -> True
    Just _ -> Yudhishthira.elemTagNameValue (LYT.TagNameValue ("Cohort#" <> show tier)) (Yudhishthira.filterExpiredTags' now (fromMaybe [] driverTag))

-- | Whether the driver is eligible for a scheduled booking of the given tier: not a scheduled ride
-- at all, within the R4 open-to-all threshold, or the tier's configured eligibility tags intersect
-- the driver's (already expiry-filtered) tags. A tier with no configured eligibility tags is open.
scheduledTierEligibleForDriver :: Bool -> Bool -> [Text] -> HashMap.HashMap ServiceTierType DVST.VehicleServiceTier -> ServiceTierType -> Bool
scheduledTierEligibleForDriver isScheduled scheduledOpenToAll driverTagTexts cityServiceTiersHashMap tier =
  not isScheduled
    || scheduledOpenToAll -- R4: within open-to-all threshold, eligibility is dropped
    || case HashMap.lookup tier cityServiceTiersHashMap >>= (.scheduleBookingListEligibilityTags) of
      Just reqTags@(_ : _) -> not . null $ DL.intersect driverTagTexts reqTags
      _ -> True

buildDriverResult ::
  NearestDriversReq ->
  HashMap.HashMap (Id Person.Driver) DPD.DriverPoolData ->
  HashMap.HashMap ServiceTierType DVST.VehicleServiceTier ->
  DriverLocation ->
  Maybe [NearestDriversResult]
buildDriverResult NearestDriversReq {..} poolDataMap cityServiceTiersHashMap location = do
  dpd <- HashMap.lookup location.driverId poolDataMap
  guard $ not dpd.blocked
  guard $ dpd.enabled
  guard $ not dpd.isDisabledReasonFlag
  guard $ dpd.subscribed
  guard $ isDriverModeEligibleHelper dpd.mode dpd.active
  guard $ isTripTypeEligibleHelper isRental isInterCity dpd
  when isAirportRequest $ guard $ dpd.enableForAirport == Just DI.ENABLED
  when dpd.onRide $ do
    guard dpd.forwardBatchingEnabled
    guard $ dpd.hasRideStarted == Just True
    guard $ isJust dpd.driverTripEndLocation
    guard $ maybe False (\tc -> tc `elem` currentRideTripCategoryValidForForwardBatching) dpd.onRideTripCategory
  when onlinePayment $ do
    guard dpd.chargesEnabled
    let effectiveMode = fromMaybe MP.LIVE dpd.bankAccountPaymentMode
        requestedMode = fromMaybe MP.LIVE paymentMode
    guard $ effectiveMode == requestedMode
  let driverPoint = LatLong {lat = location.lat, lon = location.lon}
  let (dist, mbPrevDropLat, mbPrevDropLon, mbDistToDestination) =
        if dpd.onRide
          then case dpd.driverTripEndLocation of
            Just dropLoc ->
              let distDriverToDrop = (realToFrac $ distanceBetweenInMeters driverPoint dropLoc) :: Double
                  distDropToPickup = (realToFrac $ distanceBetweenInMeters fromLocLatLong dropLoc) :: Double
               in (distDriverToDrop + distDropToPickup, Just dropLoc.lat, Just dropLoc.lon, Just $ roundToIntegral distDriverToDrop)
            Nothing -> ((realToFrac $ distanceBetweenInMeters fromLocLatLong driverPoint) :: Double, Nothing, Nothing, Nothing)
          else ((realToFrac $ distanceBetweenInMeters fromLocLatLong driverPoint) :: Double, Nothing, Nothing, Nothing)
  when dpd.onRide $ guard $ roundToIntegral dist <= nearestRadius
  let mbDefaultServiceTierForDriver = find (\vst -> dpd.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
  let softBlockedTiers = fromMaybe [] dpd.softBlockStiers
  let removeSoftBlockedTiers = filter (\stier -> stier `notElem` softBlockedTiers)
  let availableCityTiers = (.serviceTierType) <$> filter (\vst -> dpd.variant `elem` vst.allowedVehicleVariant) cityServiceTiers
  let selectedDriverServiceTiers = removeSoftBlockedTiers $ DL.intersect dpd.selectedServiceTiers availableCityTiers
  let selectedDriverServiceTiers' = filter (isTierEligibleForDriver now dpd.driverTag cityServiceTiersHashMap) selectedDriverServiceTiers
  -- Filter expired tags before matching so a stale (expired) tag can't grant scheduled eligibility,
  -- consistent with isTierEligibleForDriver above.
  let driverTagTexts = LYT.getTagNameValue . Yudhishthira.removeTagExpiry <$> Yudhishthira.filterExpiredTags' now (fromMaybe [] dpd.driverTag)
  let matchingTiers =
        filter (scheduledTierEligibleForDriver isScheduled scheduledOpenToAll driverTagTexts cityServiceTiersHashMap) $
          if null serviceTiers
            then selectedDriverServiceTiers'
            else filter (`elem` selectedDriverServiceTiers') serviceTiers
  guard $ not $ null matchingTiers
  Just $ mapMaybe (mkResultHelper now dpd location dist mbDefaultServiceTierForDriver cityServiceTiersHashMap mbPrevDropLat mbPrevDropLon mbDistToDestination) matchingTiers

mkResultHelper ::
  UTCTime ->
  DPD.DriverPoolData ->
  DriverLocation ->
  Double ->
  Maybe DVST.VehicleServiceTier ->
  HashMap.HashMap ServiceTierType DVST.VehicleServiceTier ->
  Maybe Double ->
  Maybe Double ->
  Maybe Meters ->
  ServiceTierType ->
  Maybe NearestDriversResult
mkResultHelper now dpd location dist mbDefaultServiceTierForDriver cityServiceTiersHashMap mbPrevDropLat mbPrevDropLon mbDistToDestination serviceTier = do
  serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
  let tollRouteEligible = case dpd.tollRouteBlockedTill of
        Nothing -> True
        Just blockTill -> blockTill < now
  let driverTagPrefix = if dpd.onRide then "OnRideDriver#true" else "NormalDriver#true"
  Just $
    NearestDriversResult
      { driverId = dpd.driverId,
        driverDeviceToken = dpd.deviceToken,
        language = dpd.language,
        onRide = dpd.onRide,
        distanceToDriver = roundToIntegral dist,
        variant = dpd.variant,
        serviceTier,
        serviceTierDowngradeLevel = maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver,
        isAirConditioned = serviceTierInfo.isAirConditioned,
        lat = location.lat,
        lon = location.lon,
        mode = dpd.mode,
        clientSdkVersion = dpd.clientSdkVersion,
        clientBundleVersion = dpd.clientBundleVersion,
        clientConfigVersion = dpd.clientConfigVersion,
        clientDevice = dpd.clientDevice,
        vehicleAge = getVehicleAge dpd.mYManufacturing now,
        latestScheduledBooking = dpd.latestScheduledBooking,
        latestScheduledPickup = dpd.latestScheduledPickup,
        selectedAutoAcceptTiers = fromMaybe [] dpd.selectedAutoAcceptTiers,
        driverTags = Yudhishthira.convertTags $ LYT.TagNameValueExpiry driverTagPrefix : (map LYT.TagNameValueExpiry (fromMaybe [] dpd.vehicleTags) ++ fromMaybe [] dpd.driverTag),
        score = Nothing,
        tripDistanceMinThreshold = dpd.tripDistanceMinThreshold,
        tripDistanceMaxThreshold = dpd.tripDistanceMaxThreshold,
        maxPickupDistance = dpd.maxPickupRadius,
        isPetModeEnabled = dpd.isPetModeEnabled,
        isTollRouteEligible = tollRouteEligible,
        driverGender = dpd.gender,
        previousRideDropLat = mbPrevDropLat,
        previousRideDropLon = mbPrevDropLon,
        vehicleNumber = Just dpd.registrationNo,
        fleetOwnerId = dpd.fleetOwnerId,
        distanceFromDriverToDestination = mbDistToDestination
      }

parallelRequestsFilterForDriver :: (Redis.HedisFlow m r) => NearestDriversReq -> Id Person.Driver -> m Bool
parallelRequestsFilterForDriver NearestDriversReq {..} driverId = do
  currentCount <- Redis.withMasterRedis $
    Redis.withCrossAppRedis $ do
      validCount <- Redis.zCount (DPD.mkParallelSearchRequestKey merchantId driverId) ((realToFrac . utcTimeToPOSIXSeconds) now) ((realToFrac . utcTimeToPOSIXSeconds) (addUTCTime 5000 now))
      pure (fromIntegral validCount :: Int)
  pure $ currentCount < maxParallelSearchRequests

isDriverModeEligibleHelper :: Maybe DriverInfo.DriverMode -> Bool -> Bool
isDriverModeEligibleHelper Nothing active = active
isDriverModeEligibleHelper (Just DriverInfo.SILENT) _ = True
isDriverModeEligibleHelper (Just DriverInfo.ONLINE) _ = True
isDriverModeEligibleHelper _ _ = False

isTripTypeEligibleHelper :: Bool -> Bool -> DPD.DriverPoolData -> Bool
isTripTypeEligibleHelper isRental isInterCity dpd
  | isRental = dpd.canSwitchToRental
  | isInterCity = dpd.canSwitchToInterCity
  | otherwise = dpd.canSwitchToIntraCity

filterByWalletBalance ::
  (BeamFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) =>
  NearestDriversReq ->
  Bool ->
  [NearestDriversResult] ->
  m [NearestDriversResult]
filterByWalletBalance NearestDriversReq {..} isPrepaidEnabled results = do
  afterPrepaid <-
    if isPrepaidEnabled
      then case (rideFare, prepaidSubscriptionThreshold <|> fleetPrepaidSubscriptionThreshold) of
        (Just fare, Just _) ->
          filterM
            ( \r -> do
                let mbVehicleCategory = if vehicleCategoryScopedPrepaidEnabled then Just (DV.castServiceTierToVehicleCategory r.serviceTier) else Nothing
                    (counterpartyType, ownerId, threshold) = resolveOwnerAndThreshold r
                mbBalance <- getPrepaidAvailableBalanceByOwner counterpartyType ownerId mbVehicleCategory
                otherPrepaidOfferHolds <- getPrepaidOfferHoldTotalExcluding ownerId mbSearchTryId
                let bufferedFare = fromMaybe fare (Map.lookup r.serviceTier bufferedFareByTier)
                pure $ maybe False (\b -> b - otherPrepaidOfferHolds >= bufferedFare + threshold) mbBalance
            )
            results
        _ -> pure results
      else pure results

  -- Deliberately two independent flags -- 'zeroBalanceCheckApplies' (bars a
  -- zero/negative-balance driver from cash rides outright) only needs the
  -- wallet feature on; 'cashCheckApplies' (the minimum-balance *threshold*
  -- requirement) additionally needs 'minWalletAmountForCashRides' configured
  -- (via 'cashWalletCheckEnabled'). A merchant with the wallet on but no
  -- threshold set must still get the zero-balance rule enforced.
  let zeroBalanceCheckApplies = driverWalletConfig.enableDriverWallet && shouldCheckCashWallet paymentInstrument
      cashCheckApplies = cashWalletCheckEnabled driverWalletConfig && shouldCheckCashWallet paymentInstrument
      mkCashRequirement r =
        minWalletAmountForCashRides <&> \minAmt ->
          minAmt + estimateOfferDeductions taxConfig rideFare (Map.lookup r.serviceTier bufferedFareByTier) govtCharges tollCharges parkingCharge
      airportRequirement = case airportEntryFee of
        Just fee | fee > 0 -> Just fee
        _ -> Nothing
      -- Scheduled-ride minimum wallet balance, folded into this pass so the candidate list is
      -- filtered once (combined with the cash/airport gates) rather than in a second traversal.
      applyScheduledGate = isScheduled && not scheduledOpenToAll
      anyGateApplies = zeroBalanceCheckApplies || cashCheckApplies || isJust airportRequirement || applyScheduledGate
  if not anyGateApplies
    then pure afterPrepaid
    else filterM (\r -> passesLiabilityGates (if cashCheckApplies then mkCashRequirement r else Nothing) zeroBalanceCheckApplies airportRequirement applyScheduledGate r) afterPrepaid
  where
    resolveOwnerAndThreshold r = case r.fleetOwnerId of
      Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId, fromMaybe 0 fleetPrepaidSubscriptionThreshold)
      Nothing -> (counterpartyDriver, r.driverId.getId, fromMaybe 0 prepaidSubscriptionThreshold)

    -- Fetches balance + holds once per account and evaluates whichever of the
    -- zero-balance and minimum-requirement gates apply from that single read.
    -- Previously separate 'hasPositiveCashBalance'/'checkBalance' calls
    -- independently re-fetched the same two values when both gates applied to
    -- the same account (the common case) -- 2x wallet lookups per candidate
    -- driver in this filterM, and non-atomic with each other besides.
    checkAccountGates (counterpartyType, ownerId) applyZeroBalanceGate mbRequired = do
      mbBalance <- getWalletAvailableBalanceByOwner counterpartyType ownerId
      otherOfferHolds <- getWalletOfferHoldTotalExcluding ownerId mbSearchTryId
      pure $ case mbBalance of
        Nothing -> False
        Just b ->
          let available = b - otherOfferHolds
           in (not applyZeroBalanceGate || available > 0) && maybe True (available >=) mbRequired

    checkBalance account required = checkAccountGates account False (Just required)

    passesLiabilityGates cashReq applyZeroBalanceGate airportReq applyScheduledGate r = do
      -- Scheduled-ride wallet gate first (short-circuits the cash/airport balance fetches on failure).
      scheduledOk <-
        if applyScheduledGate
          then hasMinWalletBalance counterpartyDriver minWalletAmountForScheduledRides r.driverId.getId
          else pure True
      if not scheduledOk
        then pure False
        else do
          let (cashCp, cashOwner, _) = resolveOwnerAndThreshold r
              cashAccount = (cashCp, cashOwner)
              airportAccount = (counterpartyDriver, r.driverId.getId)
          case (cashReq, airportReq) of
            (Nothing, Nothing) ->
              if applyZeroBalanceGate then checkAccountGates cashAccount True Nothing else pure True
            (Just c, Nothing) -> checkAccountGates cashAccount applyZeroBalanceGate (Just c)
            (Nothing, Just a) -> do
              zeroOk <- if applyZeroBalanceGate then checkAccountGates cashAccount True Nothing else pure True
              if zeroOk then checkBalance airportAccount a else pure False
            (Just c, Just a)
              | cashAccount == airportAccount -> checkAccountGates cashAccount applyZeroBalanceGate (Just (max c a))
              | otherwise -> do
                cashOk <- checkAccountGates cashAccount applyZeroBalanceGate (Just c)
                if cashOk then checkBalance airportAccount a else pure False
