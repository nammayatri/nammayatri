{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestForDriver (module Storage.Queries.SearchRequestForDriver, module ReExport) where

import qualified Data.Time
import qualified Domain.Types.Common
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as Beam
import Storage.Queries.SearchRequestForDriverExtra as ReExport
import Storage.Queries.Transformers.SearchRequestForDriver

findAllActiveBySRId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus -> m [Domain.Types.SearchRequestForDriver.SearchRequestForDriver])
findAllActiveBySRId requestId status = do findAllWithKV [Se.And [Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId), Se.Is Beam.status $ Se.Eq status]]

findAllActiveBySTId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus -> m [Domain.Types.SearchRequestForDriver.SearchRequestForDriver])
findAllActiveBySTId searchTryId status = do findAllWithKV [Se.And [Se.Is Beam.searchTryId $ Se.Eq (Kernel.Types.Id.getId searchTryId), Se.Is Beam.status $ Se.Eq status]]

updateDriverResponse ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.Common.SearchRequestForDriverResponse -> Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus -> Kernel.Prelude.Maybe Domain.Types.SearchRequestForDriver.NotificationSource -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver -> m ())
updateDriverResponse response status notificationSource renderedAt respondedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.response response,
      Se.Set Beam.status status,
      Se.Set Beam.notificationSource notificationSource,
      Se.Set Beam.renderedAt renderedAt,
      Se.Set Beam.respondedAt respondedAt,
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver -> m (Maybe Domain.Types.SearchRequestForDriver.SearchRequestForDriver))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRequestForDriver.SearchRequestForDriver -> m ())
updateByPrimaryKey (Domain.Types.SearchRequestForDriver.SearchRequestForDriver {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acceptanceRatio acceptanceRatio,
      Se.Set Beam.actualDistanceToPickup actualDistanceToPickup,
      Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.backendConfigVersion (fmap Kernel.Utils.Version.versionToText backendConfigVersion),
      Se.Set Beam.baseFare (Kernel.Prelude.roundToIntegral <$> baseFare),
      Se.Set Beam.baseFareAmount baseFare,
      Se.Set Beam.batchNumber batchNumber,
      Se.Set Beam.cancellationRatio cancellationRatio,
      Se.Set Beam.clientBundleVersion (fmap Kernel.Utils.Version.versionToText clientBundleVersion),
      Se.Set Beam.clientConfigVersion (fmap Kernel.Utils.Version.versionToText clientConfigVersion),
      Se.Set Beam.clientManufacturer (clientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.clientModelName (clientDevice <&> (.deviceModel)),
      Se.Set Beam.clientOsType (clientDevice <&> (.deviceType)),
      Se.Set Beam.clientOsVersion (clientDevice <&> (.deviceVersion)),
      Se.Set Beam.clientSdkVersion (fmap Kernel.Utils.Version.versionToText clientSdkVersion),
      Se.Set Beam.coinsRewardedOnGoldTierRide coinsRewardedOnGoldTierRide,
      Se.Set Beam.conditionalCharges (Kernel.Prelude.Just $ Kernel.Prelude.show conditionalCharges),
      Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.customerCancellationDues (Kernel.Prelude.Just customerCancellationDues),
      Se.Set Beam.customerTags customerTags,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverAvailableTime driverAvailableTime,
      Se.Set Beam.driverDefaultStepFee (Kernel.Prelude.roundToIntegral <$> driverDefaultStepFee),
      Se.Set Beam.driverDefaultStepFeeAmount driverDefaultStepFee,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverMaxExtraFee (Kernel.Prelude.roundToIntegral <$> driverMaxExtraFee),
      Se.Set Beam.driverMaxExtraFeeAmount driverMaxExtraFee,
      Se.Set Beam.driverMinExtraFee (Kernel.Prelude.roundToIntegral <$> driverMinExtraFee),
      Se.Set Beam.driverMinExtraFeeAmount driverMinExtraFee,
      Se.Set Beam.driverSpeed driverSpeed,
      Se.Set Beam.driverStepFee (Kernel.Prelude.roundToIntegral <$> driverStepFee),
      Se.Set Beam.driverStepFeeAmount driverStepFee,
      Se.Set Beam.driverTagScore driverTagScore,
      Se.Set Beam.driverTags driverTags,
      Se.Set Beam.durationToPickup durationToPickup,
      Se.Set Beam.estimateId estimateId,
      Se.Set Beam.fromLocGeohash fromLocGeohash,
      Se.Set Beam.goHomeRequestId (Kernel.Types.Id.getId <$> goHomeRequestId),
      Se.Set Beam.isFavourite isFavourite,
      Se.Set Beam.isForwardRequest (Kernel.Prelude.Just isForwardRequest),
      Se.Set Beam.isPartOfIntelligentPool isPartOfIntelligentPool,
      Se.Set Beam.isSafetyPlus isSafetyPlus,
      Se.Set Beam.keepHiddenForSeconds keepHiddenForSeconds,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.middleStopCount middleStopCount,
      Se.Set Beam.mode mode,
      Se.Set Beam.notificationSource notificationSource,
      Se.Set Beam.parallelSearchRequestCount parallelSearchRequestCount,
      Se.Set Beam.parcelQuantity parcelQuantity,
      Se.Set Beam.parcelType parcelType,
      Se.Set Beam.pickupZone pickupZone,
      Se.Set Beam.poolingConfigVersion poolingConfigVersion,
      Se.Set Beam.poolingLogicVersion poolingLogicVersion,
      Se.Set Beam.previousDropGeoHash previousDropGeoHash,
      Se.Set Beam.reactBundleVersion reactBundleVersion,
      Se.Set Beam.renderedAt renderedAt,
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.respondedAt respondedAt,
      Se.Set Beam.response response,
      Se.Set Beam.rideFrequencyScore rideFrequencyScore,
      Se.Set Beam.rideRequestPopupDelayDuration rideRequestPopupDelayDuration,
      Se.Set Beam.searchRequestValidTill (Data.Time.utcToLocalTime Data.Time.utc searchRequestValidTill),
      Se.Set Beam.searchTryId (Kernel.Types.Id.getId searchTryId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.straightLineDistanceToPickup straightLineDistanceToPickup,
      Se.Set Beam.totalRides (Kernel.Prelude.Just totalRides),
      Se.Set Beam.tripEstimatedDistance tripEstimatedDistance,
      Se.Set Beam.tripEstimatedDuration tripEstimatedDuration,
      Se.Set Beam.updatedAt (Just _now),
      Se.Set Beam.upgradeCabRequest upgradeCabRequest,
      Se.Set Beam.vehicleAge vehicleAge,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.vehicleServiceTier (Kernel.Prelude.Just vehicleServiceTier),
      Se.Set Beam.vehicleServiceTierName vehicleServiceTierName,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
