module Storage.Queries.Person.GetNearestDrivers
  ( getNearestDrivers,
    fetchSortedLTSCandidates,
    processCandidatesChunk,
    SortedLTSCandidate (..),
    NearestDriversResult (..),
    NearestDriversReq (..),
    estimateDeductionsFromConfig,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
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
import SharedLogic.Finance.Wallet
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
    score :: Maybe A.Value,
    tripDistanceMinThreshold :: Maybe Meters,
    tripDistanceMaxThreshold :: Maybe Meters,
    maxPickupDistance :: Maybe Meters,
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
    currentRideTripCategoryValidForForwardBatching :: [Text],
    prepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    fleetPrepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    rideFare :: Maybe HighPrecMoney,
    govtCharges :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    minWalletAmountForCashRides :: Maybe HighPrecMoney,
    paymentInstrument :: Maybe MP.PaymentInstrument,
    taxConfig :: DTC.TaxConfig,
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
  driverLocsRaw <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong nearestRadius (bool (Just allowedVehicleVariant) Nothing (null allowedVehicleVariant))
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
  let matchingTiers =
        if null serviceTiers
          then selectedDriverServiceTiers
          else filter (`elem` selectedDriverServiceTiers) serviceTiers
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
        driverTags = Yudhishthira.convertTags $ LYT.TagNameValueExpiry driverTagPrefix : (map LYT.TagNameValueExpiry (fromMaybe [] dpd.vehicleTags) ++ fromMaybe [] dpd.driverTag),
        score = Nothing,
        tripDistanceMinThreshold = dpd.tripDistanceMinThreshold,
        tripDistanceMaxThreshold = dpd.tripDistanceMaxThreshold,
        maxPickupDistance = dpd.maxPickupRadius,
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
                pure $ maybe False (>= (fare + threshold)) mbBalance
            )
            results
        _ -> pure results
      else pure results
  let cashRequirement =
        case minWalletAmountForCashRides of
          Just minAmt
            | isPrepaidEnabled && shouldCheckCashWallet paymentInstrument ->
              Just (minAmt + estimateDeductionsFromConfig taxConfig rideFare govtCharges tollCharges parkingCharge)
          _ -> Nothing
      airportRequirement = case airportEntryFee of
        Just fee | fee > 0 -> Just fee
        _ -> Nothing
  if isNothing cashRequirement && isNothing airportRequirement
    then pure afterPrepaid
    else filterM (passesLiabilityGates cashRequirement airportRequirement) afterPrepaid
  where
    resolveOwnerAndThreshold r = case r.fleetOwnerId of
      Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId, fromMaybe 0 fleetPrepaidSubscriptionThreshold)
      Nothing -> (counterpartyDriver, r.driverId.getId, fromMaybe 0 prepaidSubscriptionThreshold)

    checkBalance (counterpartyType, ownerId) required = do
      mbBalance <- getWalletBalanceByOwner counterpartyType ownerId
      pure $ maybe False (>= required) mbBalance

    passesLiabilityGates cashReq airportReq r = do
      let (cashCp, cashOwner, _) = resolveOwnerAndThreshold r
          cashAccount = (cashCp, cashOwner)
          airportAccount = (counterpartyDriver, r.driverId.getId)
      case (cashReq, airportReq) of
        (Nothing, Nothing) -> pure True
        (Just c, Nothing) -> checkBalance cashAccount c
        (Nothing, Just a) -> checkBalance airportAccount a
        (Just c, Just a)
          | cashAccount == airportAccount -> checkBalance cashAccount (max c a)
          | otherwise -> do
            cashOk <- checkBalance cashAccount c
            if cashOk then checkBalance airportAccount a else pure False

shouldCheckCashWallet :: Maybe MP.PaymentInstrument -> Bool
shouldCheckCashWallet = \case
  Nothing -> True
  Just MP.Cash -> True
  Just MP.BoothOnline -> True
  _ -> False

-- | Estimate deductions (govtCharges + TDS) from fare components.
estimateDeductionsFromConfig :: DTC.TaxConfig -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> HighPrecMoney
estimateDeductionsFromConfig taxConfig rideFare govtCharges_ tollCharges_ parkingCharge_ =
  case rideFare of
    Nothing -> 0
    Just totalFare ->
      let gstAmount = fromMaybe 0 govtCharges_
          tollAmount = fromMaybe 0 tollCharges_
          parkingAmount = fromMaybe 0 parkingCharge_
          baseFare = totalFare - gstAmount - tollAmount - parkingAmount
          tdsRate = Just taxConfig.invalidPanTdsRate.rate
       in gstAmount + estimateWalletDeductions tdsRate baseFare
