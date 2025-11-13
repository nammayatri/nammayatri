{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Notification
  ( sendDummyRideRequestToDriver,
    triggerDummyRideRequest,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as Common
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchReqLocation as DSSL
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Type as SLT
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import qualified Tools.Notifications as TN
import Utils.Common.Cac.KeyNameConstants

--------------------------------------------------------------------------------------------------

sendDummyRideRequestToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
sendDummyRideRequestToDriver merchantShortId opCity driverId = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)

  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access check
  unless (merchantOperatingCity.id == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  triggerDummyRideRequest driver merchantOperatingCity.id True

triggerDummyRideRequest ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasKafkaProducer r
  ) =>
  DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  Bool ->
  m APISuccess
triggerDummyRideRequest driver merchantOperatingCityId isDashboardTrigger = do
  vehicle <- B.runInReplica $ QVehicle.findById driver.id >>= fromMaybeM (VehicleDoesNotExist driver.id.getId)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOperatingCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigDoesNotExist merchantOperatingCityId.getId)
  let dummyFromLocation = transporterConfig.dummyFromLocation
      dummyToLocation = transporterConfig.dummyToLocation
      dummyShowDriverAdditions = fromMaybe True transporterConfig.dummyShowDriverAdditions
  let isValueAddNP = True

  now <- getCurrentTime
  let entityData = mkDummyNotificationEntityData (Just driver.merchantId) (Just merchantOperatingCityId) now vehicle.variant dummyFromLocation dummyToLocation dummyShowDriverAdditions isValueAddNP
  notificationData <- TN.buildSendSearchRequestNotificationData merchantOperatingCityId driver.id driver.deviceToken entityData EmptyDynamicParam Nothing
  logDebug $ "Sending dummy notification to driver:-" <> show driver.id <> ",entityData:-" <> show entityData <> ",triggeredByDashboard:-" <> show isDashboardTrigger
  let otherMerchantIds = ["840327a8-f17c-4d7c-8199-a583cfaadc5f", "7e6a2982-f8b5-4c67-b8af-bf41f1b4a2c9", "8c91f173-a0e3-4c5b-b3a1-2a58d00f29b2"] -- Array Contents are : [Dev/Master , UAT , Prod]
  let fallBackCity = bool (TN.getNewMerchantOpCityId driver.clientSdkVersion merchantOperatingCityId) (TN.cityFallback driver.clientSdkVersion merchantOperatingCityId) (driver.merchantId `elem` otherMerchantIds) -- TODO: Remove this fallback once YATRI_PARTNER_APP is updated To Newer Version
  void $ TN.sendSearchRequestToDriverNotification driver.merchantId fallBackCity driver.id notificationData
  pure Success

mkDummyNotificationEntityData ::
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  UTCTime ->
  DVeh.VehicleVariant ->
  DLoc.DummyLocationInfo ->
  DLoc.DummyLocationInfo ->
  Bool ->
  Bool ->
  USRD.SearchRequestForDriverAPIEntity
mkDummyNotificationEntityData mbMerchantId mbMerchantOpCityId now driverVehicle fromLocData toLocData dummyShowDriverAdditions isValueAddNP =
  let searchRequestValidTill = addUTCTime 30 now
      fromLocation = mkDummySearchReqFromLocation mbMerchantId mbMerchantOpCityId now fromLocData
      toLocation = Just $ mkDummySearchReqToLocation mbMerchantId mbMerchantOpCityId now toLocData
      -- newFromLocation = mkDummyFromLocation now fromLocData
      -- newToLocation = Just $ mkDummyToLocation now toLocData
      mkDummyPrice (amountInt :: Int) = PriceAPIEntity (toHighPrecMoney amountInt) INR
      (driverMinExtraFee, driverMaxExtraFee, driverMinExtraFeeWithCurrency, driverMaxExtraFeeWithCurrency, driverDefaultStepFeeWithCurrency, driverStepFeeWithCurrency) =
        if dummyShowDriverAdditions
          then (Just (Money 0), Just (Money 20), Just $ mkDummyPrice 0, Just $ mkDummyPrice 20, Just $ mkDummyPrice 10, Just $ mkDummyPrice 10)
          else (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
   in USRD.SearchRequestForDriverAPIEntity
        { searchRequestId = Id fromLocData.dummyId,
          searchTryId = Id fromLocData.dummyId,
          startTime = now,
          distance = Just fromLocData.distance,
          congestionCharges = Nothing,
          petCharges = Nothing,
          distanceWithUnit = Just $ convertMetersToDistance Meter fromLocData.distance,
          distanceToPickup = Meters 149,
          distanceToPickupWithUnit = Distance 149.0 Meter,
          durationToPickup = Seconds 65,
          baseFare = Money fromLocData.baseFare,
          baseFareWithCurrency = mkDummyPrice fromLocData.baseFare,
          driverLatLong = LatLong {lat = fromLocData.lat, lon = fromLocData.lon},
          rideRequestPopupDelayDuration = Seconds 0,
          keepHiddenForSeconds = Seconds 0,
          requestedVehicleVariant = driverVehicle,
          airConditioned = Nothing,
          vehicleServiceTier = Just $ show $ DVeh.castVariantToServiceTier driverVehicle,
          bapName = Nothing,
          bapLogo = Nothing,
          customerExtraFee = Nothing,
          customerExtraFeeWithCurrency = Nothing,
          specialLocationTag = Nothing,
          tollCharges = Nothing,
          tollChargesWithCurrency = Nothing,
          tollNames = Nothing,
          disabilityTag = Nothing,
          billingCategory = SLT.PERSONAL,
          goHomeRequestId = Nothing,
          isTranslated = False,
          -- customerCancellationDues = 0,
          -- customerCancellationDuesWithCurrency = mkDummyPrice 0,
          tripCategory = DTC.OneWay DTC.OneWayOnDemandDynamicOffer,
          duration = Just (Seconds 300),
          pickupZone = False, -- TODO: make it dynamic ?
          specialZoneExtraTip = Nothing,
          specialZoneExtraTipWithCurrency = Nothing,
          driverPickUpCharges = Nothing,
          parkingCharge = Nothing,
          driverPickUpChargesWithCurrency = Nothing,
          driverDefaultStepFeeWithCurrencyV2 = driverDefaultStepFeeWithCurrency,
          useSilentFCMForForwardBatch = False,
          isOnRide = False,
          isFavourite = Just False,
          isReferredRideReq = Nothing,
          roundTrip = Just False,
          middleStopCount = 0,
          parcelQuantity = Nothing,
          parcelType = Nothing,
          conditionalCharges = [],
          isSafetyPlus = False,
          safetyPlusCharges = Nothing,
          coinsRewardedOnGoldTierRide = Nothing,
          cancellationCharges = Nothing,
          ..
        }

mkDummySearchReqFromLocation ::
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  UTCTime ->
  DLoc.DummyLocationInfo ->
  DSSL.SearchReqLocation
mkDummySearchReqFromLocation mbMerchantId mbMerchantOpCityId now fromLocData =
  let DLoc.LocationAddress {..} = mkDummyFromAddress fromLocData
   in DSSL.SearchReqLocation
        { id = Id fromLocData.dummyId,
          lat = fromLocData.lat,
          lon = fromLocData.lon,
          full_address = fullAddress,
          createdAt = now,
          updatedAt = now,
          merchantId = mbMerchantId,
          merchantOperatingCityId = mbMerchantOpCityId,
          ..
        }

mkDummySearchReqToLocation ::
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  UTCTime ->
  DLoc.DummyLocationInfo ->
  DSSL.SearchReqLocation
mkDummySearchReqToLocation mbMerchantId mbMerchantOpCityId now toLocData =
  let DLoc.LocationAddress {..} = mkDummyToAddress toLocData
   in DSSL.SearchReqLocation
        { id = Id toLocData.dummyId,
          lat = toLocData.lat,
          lon = toLocData.lon,
          full_address = fullAddress,
          createdAt = now,
          updatedAt = now,
          merchantId = mbMerchantId,
          merchantOperatingCityId = mbMerchantOpCityId,
          ..
        }

-- mkDummyFromLocation :: UTCTime -> DLoc.DummyLocationInfo -> DLoc.Location
-- mkDummyFromLocation now fromLocData =
--   DLoc.Location
--     { id = Id fromLocData.dummyId,
--       address = mkDummyFromAddress fromLocData,
--       lat = fromLocData.lat,
--       lon = fromLocData.lon,
--       createdAt = now,
--       updatedAt = now
--     }

-- mkDummyToLocation :: UTCTime -> DLoc.DummyLocationInfo -> DLoc.Location
-- mkDummyToLocation now toLocData =
--   DLoc.Location
--     { id = Id toLocData.dummyId,
--       address = mkDummyToAddress toLocData,
--       lat = toLocData.lat,
--       lon = toLocData.lon,
--       createdAt = now,
--       updatedAt = now
--     }

mkDummyFromAddress :: DLoc.DummyLocationInfo -> DLoc.LocationAddress
mkDummyFromAddress DLoc.DummyLocationInfo {..} = DLoc.LocationAddress {..}

mkDummyToAddress :: DLoc.DummyLocationInfo -> DLoc.LocationAddress
mkDummyToAddress DLoc.DummyLocationInfo {..} = DLoc.LocationAddress {..}

--------------------------------------------------------------------------------------------------
