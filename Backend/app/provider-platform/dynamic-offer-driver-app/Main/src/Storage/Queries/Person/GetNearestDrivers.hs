module Storage.Queries.Person.GetNearestDrivers
  ( getNearestDrivers,
    NearestDriversResult (..),
    NearestDriversReq (..),
    estimateDeductionsFromConfig,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import Domain.Types
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import Domain.Types.Merchant
import Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as VC
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
    paymentMode :: Maybe MP.PaymentMode
  }

getNearestDrivers ::
  (BeamFlow m r, MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasShortDurationRetryCfg r c) =>
  NearestDriversReq ->
  ([Id Person.Driver] -> m [DPD.DriverPoolData]) -> -- pool data fetcher (breaks import cycle)
  m [NearestDriversResult]
getNearestDrivers NearestDriversReq {..} fetchPoolData = do
  let allowedCityServiceTiers = filter (\cvst -> cvst.serviceTierType `elem` serviceTiers) cityServiceTiers
      allowedVehicleVariant = DL.nub (concatMap (.allowedVehicleVariant) allowedCityServiceTiers)

  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let isPrepaidEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled

  -- Step 1: Get driver locations from LTS
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong nearestRadius (bool (Just allowedVehicleVariant) Nothing (null allowedVehicleVariant))
  logDebug $ "DriverPool[1-LTS] " <> show (length driverLocs) <> " drivers within " <> show nearestRadius <> "m"

  -- Step 2: Fetch pool data via injected function (lazy-build from DB on cold start)
  poolDataList <- fetchPoolData (driverLocs <&> (.driverId))
  let poolDataMap = HashMap.fromList $ (\dpd -> (dpd.driverId, dpd)) <$> poolDataList
      driversWithData = length poolDataList
      driversMissing = length driverLocs - driversWithData
  logDebug $ "DriverPool[2-PoolData] found=" <> show driversWithData <> " missing=" <> show driversMissing

  -- Step 3: Filter and build results using pool data + location
  let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers

  -- Debug: count drivers dropped at each filter stage
  logDebug $ "DriverPool[3-Eligibility] " <> formatEligibilityLog (foldl' countFilters (0, 0, 0, 0, 0, 0, 0) $ mapMaybe (`HashMap.lookup` poolDataMap) (driverLocs <&> (.driverId)))

  let results = concat $ mapMaybe (buildDriverResult poolDataMap cityServiceTiersHashMap) driverLocs
      offRideCount = length $ filter (not . (.onRide)) results
      onRideCount = length $ filter (.onRide) results
  logDebug $ "DriverPool[4-ServiceTier] offRide=" <> show offRideCount <> " onRide=" <> show onRideCount <> " total=" <> show (length results)

  -- Step 4: Wallet balance checks (still external service calls, can't move to Redis)
  filteredResults <- filterByWalletBalance isPrepaidEnabled results
  logDebug $ "DriverPool[5-Final] afterWallet=" <> show (length filteredResults) <> " (dropped " <> show (length results - length filteredResults) <> " by wallet check)"

  return filteredResults
  where
    buildDriverResult poolDataMap cityServiceTiersHashMap location = do
      dpd <- HashMap.lookup location.driverId poolDataMap

      -- Core eligibility filters
      guard $ not dpd.blocked
      guard $ dpd.subscribed
      guard $ isDriverModeEligible dpd.mode dpd.active
      guard $ isTripTypeEligible dpd
      -- On-ride drivers: only pass through if eligible for forward batching
      when dpd.onRide $ do
        guard dpd.forwardBatchingEnabled
        guard $ dpd.hasRideStarted == Just True
        guard $ isJust dpd.driverTripEndLocation
        guard $ maybe False (\tc -> tc `elem` currentRideTripCategoryValidForForwardBatching) dpd.onRideTripCategory

      when onlinePayment $ guard dpd.chargesEnabled

      -- For on-ride drivers, compute two-leg distance (driver→drop + drop→pickup)
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

      -- For on-ride drivers, validate combined distance against radius
      when dpd.onRide $ guard $ roundToIntegral dist <= nearestRadius

      let mbDefaultServiceTierForDriver = find (\vst -> dpd.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let softBlockedTiers = fromMaybe [] dpd.softBlockStiers
      let removeSoftBlockedTiers = filter (\stier -> stier `notElem` softBlockedTiers)

      -- Service tier selection with AC usage restriction (replaces selectVehicleTierForDriverWithUsageRestriction)
      let availableTiersWithUsageRestriction =
            map (checkUsageRestriction dpd) $
              filter (\vst -> dpd.variant `elem` vst.allowedVehicleVariant) cityServiceTiers
      let ifUsageRestricted = any snd availableTiersWithUsageRestriction
      let selectedDriverServiceTiers =
            removeSoftBlockedTiers $
              if ifUsageRestricted
                then (.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction)
                else DL.intersect dpd.selectedServiceTiers ((.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction))

      let matchingTiers =
            if null serviceTiers
              then selectedDriverServiceTiers
              else filter (`elem` selectedDriverServiceTiers) serviceTiers

      Just $ mapMaybe (mkResult dpd location dist mbDefaultServiceTierForDriver cityServiceTiersHashMap mbPrevDropLat mbPrevDropLon mbDistToDestination) matchingTiers

    -- AC / luggage / rating usage restriction check per service tier (equivalent to selectVehicleTierForDriverWithUsageRestriction)
    checkUsageRestriction :: DPD.DriverPoolData -> DVST.VehicleServiceTier -> (DVST.VehicleServiceTier, Bool)
    checkUsageRestriction dpd vst =
      let luggageCheck = compareNumber dpd.luggageCapacity vst.luggageCapacity
          acCheck =
            (vst.vehicleCategory == Just VC.AMBULANCE)
              || ( compareNumber vst.airConditionedThreshold dpd.airConditionScore
                     && (isNothing vst.airConditionedThreshold || dpd.airConditioned /= Just False)
                 )
          vehicleRatingCheck = compareNumber dpd.vehicleRating vst.vehicleRating
          usageRestricted = not (luggageCheck && acCheck && vehicleRatingCheck)
       in (vst, usageRestricted)

    compareNumber :: Ord a => Maybe a -> Maybe a -> Bool
    compareNumber mbX mbY = case (mbX, mbY) of
      (Just x, Just y) -> x >= y
      _ -> True

    mkResult dpd location dist mbDefaultServiceTierForDriver cityServiceTiersHashMap mbPrevDropLat mbPrevDropLon mbDistToDestination serviceTier = do
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
            distanceFromDriverToDestination = mbDistToDestination
          }

    isDriverModeEligible :: Maybe DriverInfo.DriverMode -> Bool -> Bool
    isDriverModeEligible Nothing active = active
    isDriverModeEligible (Just DriverInfo.SILENT) _ = True
    isDriverModeEligible (Just DriverInfo.ONLINE) _ = True
    isDriverModeEligible _ _ = False

    isTripTypeEligible :: DPD.DriverPoolData -> Bool
    isTripTypeEligible dpd
      | isRental = dpd.canSwitchToRental
      | isInterCity = dpd.canSwitchToInterCity
      | otherwise = dpd.canSwitchToIntraCity

    -- Debug: count how many drivers fail at each filter stage
    countFilters :: (Int, Int, Int, Int, Int, Int, Int) -> DPD.DriverPoolData -> (Int, Int, Int, Int, Int, Int, Int)
    countFilters (total, blocked, unsubscribed, modeOff, tripType, onRideInelig, noPay) dpd =
      ( total + 1,
        blocked + if dpd.blocked then 1 else 0,
        unsubscribed + if not dpd.subscribed then 1 else 0,
        modeOff + if not (isDriverModeEligible dpd.mode dpd.active) then 1 else 0,
        tripType + if not (isTripTypeEligible dpd) then 1 else 0,
        onRideInelig + if dpd.onRide && not dpd.forwardBatchingEnabled then 1 else 0,
        noPay + if onlinePayment && not dpd.chargesEnabled then 1 else 0
      )

    formatEligibilityLog :: (Int, Int, Int, Int, Int, Int, Int) -> Text
    formatEligibilityLog (total, blocked, unsubscribed, modeOff, tripType, onRideInelig, noPay) =
      "total=" <> show total
        <> " blocked="
        <> show blocked
        <> " unsubscribed="
        <> show unsubscribed
        <> " modeOff="
        <> show modeOff
        <> " tripTypeInelig="
        <> show tripType
        <> " onRideInelig="
        <> show onRideInelig
        <> " noPayment="
        <> show noPay

    -- Wallet balance filtering — still requires external service calls
    filterByWalletBalance :: (BeamFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r) => Bool -> [NearestDriversResult] -> m [NearestDriversResult]
    filterByWalletBalance isPrepaidEnabled results
      | not isPrepaidEnabled = pure results
      | otherwise = do
        -- Prepaid balance check
        results' <- case (rideFare, prepaidSubscriptionThreshold <|> fleetPrepaidSubscriptionThreshold) of
          (Just fare, Just threshold) ->
            filterM
              ( \r -> do
                  mbBalance <- getPrepaidAvailableBalanceByOwner counterpartyDriver r.driverId.getId
                  pure $ maybe False (>= (fare + threshold)) mbBalance
              )
              results
          _ -> pure results
        -- Cash ride minimum wallet check
        case minWalletAmountForCashRides of
          Just minAmt | shouldCheckCashWallet -> do
            let estimatedDeductions = estimateDeductionsFromConfig taxConfig rideFare govtCharges tollCharges parkingCharge
                requiredBalance = minAmt + estimatedDeductions
            filterM
              ( \r -> do
                  mbBalance <- getWalletBalanceByOwner counterpartyDriver r.driverId.getId
                  pure $ maybe False (>= requiredBalance) mbBalance
              )
              results'
          _ -> pure results'

    shouldCheckCashWallet = case paymentInstrument of
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
          tdsRate = Just taxConfig.invalidPanTdsRate
       in gstAmount + estimateWalletDeductions tdsRate baseFare
