module Storage.Queries.Person.GetNearestDrivers
  ( getNearestDrivers,
    NearestDriversResult (..),
    NearestDriversReq (..),
    hasSufficientBalance,
    filterDriversBySufficientBalance,
    filterDriversByMinWalletBalance,
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import Domain.Types
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.Driver.DriverInformation as DIAPI
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantPaymentMethod as MP
import qualified Domain.Types.FleetDriverAssociation as FDA
import qualified Domain.Types.FleetOwnerInformation as FOI
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant as DV
import Domain.Utils
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.Prepaid
import SharedLogic.Finance.Wallet
import SharedLogic.VehicleServiceTier
import Storage.Beam.Finance ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int
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
    reactBundleVersion :: Maybe Text,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    driverTags :: A.Value,
    score :: Maybe A.Value,
    tripDistanceMinThreshold :: Maybe Meters,
    tripDistanceMaxThreshold :: Maybe Meters,
    maxPickupDistance :: Maybe Meters,
    isTollRouteEligible :: Bool -- True if tollRouteBlockedTill is Nothing or < now
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
    prepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    fleetPrepaidSubscriptionThreshold :: Maybe HighPrecMoney,
    rideFare :: Maybe HighPrecMoney,
    minWalletAmountForCashRides :: Maybe HighPrecMoney,
    paymentInstrument :: Maybe MP.PaymentInstrument,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    now :: UTCTime,
    paymentMode :: Maybe MP.PaymentMode
  }

getNearestDrivers ::
  (BeamFlow m r, MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, EsqDBFlow m r, CacheFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  NearestDriversReq ->
  m [NearestDriversResult]
getNearestDrivers NearestDriversReq {..} = do
  let allowedCityServiceTiers = filter (\cvst -> cvst.serviceTierType `elem` serviceTiers) cityServiceTiers
      allowedVehicleVariant = DL.nub (concatMap (.allowedVehicleVariant) allowedCityServiceTiers)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocLatLong nearestRadius (bool (Just allowedVehicleVariant) Nothing (null allowedVehicleVariant))
  driverInfos_ <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) True False isRental isInterCity
  driverInfosPrepaid <- filterDriversBySufficientBalance merchant rideFare fleetPrepaidSubscriptionThreshold prepaidSubscriptionThreshold driverInfos_
  driverInfos <- filterDriversByMinWalletBalance merchant minWalletAmountForCashRides paymentInstrument driverInfosPrepaid
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  -- driverStats <- QDriverStats.findAllByDriverIds drivers
  logDebug $ "MetroWarriorDebugging Result:- getNearestDrivers --------person tags driverInfos----" <> show (DIAPI.convertToDriverInfoAPIEntity <$> driverInfos)
  driverBankAccounts <-
    if onlinePayment
      then QDBA.getDriverOrFleetBankAccounts paymentMode (driverLocs <&> (.driverId))
      else return []

  logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos_) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  let res = linkArrayList driverLocs driverInfos vehicle drivers driverBankAccounts
  logDebug $ "GetNearestDrivers Result:- " <> show (length res)
  logDebug $ "MetroWarriorDebugging Result:- getNearestDrivers --------person tags res----" <> show res
  return res
  where
    linkArrayList driverLocations driverInformations vehicles persons driverBankAccounts =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          -- driverStatsHashMap = HashMap.fromList $ (\stats -> (stats.driverId, stats)) <$> driverStats
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverBankAccountHashMap = HashMap.fromList $ mapMaybe (\(personId, dba) -> if dba.chargesEnabled then Just (personId, dba.accountId) else Nothing) driverBankAccounts
       in concat $ mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap) driverLocations

    buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      -- driverStats <- HashMap.lookup driverId' driverStatsHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      when onlinePayment $ do
        guard (isJust $ HashMap.lookup driverId' driverBankAccountHashMap)
      let dist = (realToFrac $ distanceBetweenInMeters fromLocLatLong $ LatLong {lat = location.lat, lon = location.lon}) :: Double
      -- ideally should be there inside the vehicle.selectedServiceTiers but still to make sure we have a default service tier for the driver
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let mbDefaultServiceTierForDriver = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let availableTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction False info vehicle cityServiceTiers
      let ifUsageRestricted = any (\(_, usageRestricted) -> usageRestricted) availableTiersWithUsageRestriction
      let softBlockedTiers = fromMaybe [] info.softBlockStiers
      let removeSoftBlockedTiers = filter (\stier -> stier `notElem` softBlockedTiers)
      let upgradedTiers = DL.intersect ((.tier) <$> fromMaybe [] vehicle.ruleBasedUpgradeTiers) ((.tier) <$> fromMaybe [] info.ruleBasedUpgradeTiers)
      let addRuleBasedUpgradeTiers existing = DL.nub $ (filter (\tier -> maybe False (\tierInfo -> vehicle.variant `elem` tierInfo.allowedVehicleVariant) (HashMap.lookup tier cityServiceTiersHashMap)) upgradedTiers) <> existing
      let selectedDriverServiceTiers =
            removeSoftBlockedTiers $
              addRuleBasedUpgradeTiers $
                if ifUsageRestricted
                  then do
                    (.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction) -- no need to check for user selection
                  else do
                    DL.intersect vehicle.selectedServiceTiers ((.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction))
      if null serviceTiers
        then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap) selectedDriverServiceTiers
        else do
          Just $
            mapMaybe
              ( \serviceTier -> do
                  if serviceTier `elem` selectedDriverServiceTiers
                    then mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier
                    else Nothing
              )
              serviceTiers
      where
        mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          -- Driver is eligible for toll routes if not blocked or block has expired
          let tollRouteEligible = case info.tollRouteBlockedTill of
                Nothing -> True
                Just blockTill -> blockTill < now
          Just $
            NearestDriversResult
              { driverId = cast person.id,
                driverDeviceToken = person.deviceToken,
                language = person.language,
                onRide = info.onRide,
                distanceToDriver = roundToIntegral dist,
                variant = vehicle.variant,
                serviceTier,
                serviceTierDowngradeLevel = maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver,
                isAirConditioned = serviceTierInfo.isAirConditioned,
                lat = location.lat,
                lon = location.lon,
                mode = info.mode,
                clientSdkVersion = person.clientSdkVersion,
                clientBundleVersion = person.clientBundleVersion,
                clientConfigVersion = person.clientConfigVersion,
                reactBundleVersion = person.reactBundleVersion,
                clientDevice = person.clientDevice,
                vehicleAge = getVehicleAge vehicle.mYManufacturing now,
                backendConfigVersion = person.backendConfigVersion,
                backendAppVersion = person.backendAppVersion,
                latestScheduledBooking = info.latestScheduledBooking,
                latestScheduledPickup = info.latestScheduledPickup,
                driverTags = Yudhishthira.convertTags $ LYT.TagNameValueExpiry "NormalDriver#true" : (map LYT.TagNameValueExpiry (fromMaybe [] vehicle.vehicleTags) ++ fromMaybe [] person.driverTag),
                score = Nothing,
                tripDistanceMinThreshold = info.tripDistanceMinThreshold,
                tripDistanceMaxThreshold = info.tripDistanceMaxThreshold,
                maxPickupDistance = info.maxPickupRadius,
                isTollRouteEligible = tollRouteEligible
              }

hasSufficientBalance ::
  (BeamFlow m r, MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  HashMap.HashMap (Id Person.Driver) FDA.FleetDriverAssociation ->
  HashMap.HashMap (Id Person) FOI.FleetOwnerInformation ->
  DI.DriverInformation ->
  m Bool
hasSufficientBalance fare fleetThreshold driverThreshold fleetAssociationMap fleetOwnerMap driver =
  case HashMap.lookup driver.driverId fleetAssociationMap of
    Just fleetAssociation ->
      case HashMap.lookup (Id fleetAssociation.fleetOwnerId) fleetOwnerMap of
        Just fleetOwner ->
          do
            mbBalance <- getPrepaidAvailableBalanceByOwner counterpartyFleetOwner fleetOwner.fleetOwnerPersonId.getId
            pure $ case mbBalance of
              Just balance -> balance >= (fare + fromMaybe 0 fleetThreshold)
              _ -> False
        Nothing -> do
          logError "Fleet owner not found for an existing fleet driver association."
          pure False
    Nothing ->
      do
        mbBalance <- getPrepaidAvailableBalanceByOwner counterpartyDriver driver.driverId.getId
        pure $ case mbBalance of
          Just balance -> balance >= (fare + fromMaybe 0 driverThreshold)
          _ -> False

filterDriversBySufficientBalance ::
  (BeamFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  Merchant ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  Maybe HighPrecMoney ->
  [DI.DriverInformation] ->
  m [DI.DriverInformation]
filterDriversBySufficientBalance merchant rideFare fleetPrepaidSubscriptionThreshold prepaidSubscriptionThreshold driverInfos_ = do
  let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  if not isPrepaidSubscriptionAndWalletEnabled
    then pure driverInfos_
    else case rideFare of
      Just fare -> do
        fleetAssociations <- QFDA.findAllByDriverIds (driverInfos_ <&> (.driverId))
        let fleetOwnerIds = fleetAssociations <&> (.fleetOwnerId)
        fleetOwners <- QFOI.findAllByPrimaryKeys (map Id fleetOwnerIds)
        let fleetOwnerMap = HashMap.fromList $ map (\fo -> (fo.fleetOwnerPersonId, fo)) fleetOwners
            fleetAssociationMap = HashMap.fromList $ map (\fa -> (fa.driverId, fa)) fleetAssociations
        filterM (hasSufficientBalance fare fleetPrepaidSubscriptionThreshold prepaidSubscriptionThreshold fleetAssociationMap fleetOwnerMap) driverInfos_
      Nothing -> pure driverInfos_

filterDriversByMinWalletBalance ::
  (BeamFlow m r, EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  Merchant ->
  Maybe HighPrecMoney ->
  Maybe MP.PaymentInstrument ->
  [DI.DriverInformation] ->
  m [DI.DriverInformation]
filterDriversByMinWalletBalance merchant minWalletAmountForCashRides paymentInstrument driverInfos_ = do
  let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  if not isPrepaidSubscriptionAndWalletEnabled
    then pure driverInfos_
    else case minWalletAmountForCashRides of
      Just minAmt | shouldCheckWalletBalance paymentInstrument -> filterM (hasMinWalletBalance minAmt) driverInfos_
      _ -> pure driverInfos_
  where
    shouldCheckWalletBalance = \case
      Nothing -> True
      Just MP.Cash -> True
      Just MP.BoothOnline -> True
      _ -> False

    hasMinWalletBalance minAmt driver = do
      mbBalance <- getWalletBalanceByOwner counterpartyDriver driver.driverId.getId
      pure $ maybe False (>= minAmt) mbBalance
