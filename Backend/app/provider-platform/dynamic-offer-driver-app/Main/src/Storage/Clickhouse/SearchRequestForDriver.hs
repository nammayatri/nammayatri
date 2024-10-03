{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Clickhouse.SearchRequestForDriver where

import qualified Data.Time as T
import qualified Domain.Types.Common as DI
import qualified Domain.Types.DriverGoHomeRequest as DP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DServiceTierType
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import qualified Kernel.Types.Common as KC
import Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import Kernel.Utils.Common

data SearchRequestForDriverT f = SearchRequestForDriverT
  { acceptanceRatio :: C f (Maybe Double),
    actualDistanceToPickup :: C f KC.Meters,
    airConditioned :: C f (Maybe Bool),
    backendAppVersion :: C f (Maybe Text),
    backendConfigVersion :: C f (Maybe Kernel.Types.Version.Version),
    baseFare :: C f (Maybe KC.HighPrecMoney),
    batchNumber :: C f Int,
    cancellationRatio :: C f (Maybe Double),
    clientBundleVersion :: C f (Maybe Kernel.Types.Version.Version),
    clientConfigVersion :: C f (Maybe Kernel.Types.Version.Version),
    clientDevice :: C f (Maybe Kernel.Types.Version.Device),
    clientSdkVersion :: C f (Maybe Kernel.Types.Version.Version),
    createdAt :: C f CH.DateTime,
    currency :: C f KC.Currency,
    customerCancellationDues :: C f KC.HighPrecMoney,
    distanceUnit :: C f KC.DistanceUnit,
    driverAvailableTime :: C f (Maybe Double),
    driverDefaultStepFee :: C f (Maybe KC.HighPrecMoney),
    driverId :: C f (Id DP.Person),
    driverMaxExtraFee :: C f (Maybe KC.HighPrecMoney),
    driverMinExtraFee :: C f (Maybe KC.HighPrecMoney),
    driverSpeed :: C f (Maybe Double),
    driverStepFee :: C f (Maybe KC.HighPrecMoney),
    durationToPickup :: C f KC.Seconds,
    estimateId :: C f (Maybe Text),
    fromLocGeohash :: C f (Maybe Text),
    goHomeRequestId :: C f (Maybe (Id DP.DriverGoHomeRequest)),
    id :: C f (Id DSRD.SearchRequestForDriver),
    isForwardRequest :: C f Bool,
    isPartOfIntelligentPool :: C f Bool,
    keepHiddenForSeconds :: C f KC.Seconds,
    lat :: C f (Maybe Double),
    lon :: C f (Maybe Double),
    merchantId :: C f (Maybe (Id DM.Merchant)),
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    mode :: C f (Maybe DI.DriverMode),
    notificationSource :: C f (Maybe DSRD.NotificationSource),
    parallelSearchRequestCount :: C f (Maybe Int),
    pickupZone :: C f Bool,
    previousDropGeoHash :: C f (Maybe Text),
    renderedAt :: C f (Maybe UTCTime),
    requestId :: C f (Id DSR.SearchRequest),
    respondedAt :: C f (Maybe UTCTime),
    response :: C f (Maybe DSRD.SearchRequestForDriverResponse),
    rideFrequencyScore :: C f (Maybe Double),
    rideRequestPopupDelayDuration :: C f KC.Seconds,
    searchRequestValidTill :: C f UTCTime,
    searchTryId :: C f (Id DST.SearchTry),
    startTime :: C f UTCTime,
    status :: C f DSRD.DriverSearchRequestStatus,
    straightLineDistanceToPickup :: C f KC.Meters,
    totalRides :: C f Int,
    updatedAt :: C f (Maybe UTCTime),
    vehicleAge :: C f (Maybe Kernel.Types.Time.Months),
    vehicleServiceTier :: C f DServiceTierType.ServiceTierType,
    vehicleServiceTierName :: C f (Maybe Text),
    vehicleVariant :: C f DV.VehicleVariant
  }
  deriving (Generic)

deriving instance Show SearchRequestForDriver

instance ClickhouseValue KC.Meters

instance ClickhouseValue Kernel.Types.Version.Version

instance ClickhouseValue Kernel.Types.Version.Device

instance ClickhouseValue DistanceUnit

instance ClickhouseValue Seconds

instance ClickhouseValue DSRD.NotificationSource

instance ClickhouseValue DSRD.SearchRequestForDriverResponse

instance ClickhouseValue DSRD.DriverSearchRequestStatus

instance ClickhouseValue Months

instance ClickhouseValue DV.VehicleVariant

instance Read Kernel.Types.Version.Version where
  readsPrec _ = reads

searchRequestForDriverTTable :: SearchRequestForDriverT (FieldModification SearchRequestForDriverT)
searchRequestForDriverTTable =
  SearchRequestForDriverT
    { acceptanceRatio = "acceptance_ratio",
      actualDistanceToPickup = "actual_distance_to_pickup",
      airConditioned = "air_conditioned",
      backendAppVersion = "backend_app_version",
      backendConfigVersion = "backend_config_version",
      baseFare = "base_fare",
      batchNumber = "batch_number",
      cancellationRatio = "cancellation_ratio",
      clientBundleVersion = "client_bundle_version",
      clientConfigVersion = "client_config_version",
      clientDevice = "client_device",
      clientSdkVersion = "client_sdk_version",
      createdAt = "created_at",
      currency = "currency",
      customerCancellationDues = "customer_cancellation_dues",
      distanceUnit = "distance_unit",
      driverAvailableTime = "driver_available_time",
      driverDefaultStepFee = "driver_default_step_fee",
      driverId = "driver_id",
      driverMaxExtraFee = "driver_max_extra_fee",
      driverMinExtraFee = "driver_min_extra_fee",
      driverSpeed = "driver_speed",
      driverStepFee = "driver_step_fee",
      durationToPickup = "duration_to_pickup",
      estimateId = "estimate_id",
      fromLocGeohash = "from_loc_geohash",
      goHomeRequestId = "go_home_request_id",
      id = "id",
      isForwardRequest = "is_forward_request",
      isPartOfIntelligentPool = "is_part_of_intelligent_pool",
      keepHiddenForSeconds = "keep_hidden_for_seconds",
      lat = "lat",
      lon = "lon",
      merchantId = "merchant_id",
      merchantOperatingCityId = "merchant",
      mode = "mode",
      notificationSource = "notification_source",
      parallelSearchRequestCount = "parallel_search_request_count",
      pickupZone = "pickup_zone",
      previousDropGeoHash = "previous_drop_geohash",
      renderedAt = "rendered_at",
      requestId = "search_request_id",
      respondedAt = "responded_at",
      response = "response",
      rideFrequencyScore = "ride_frequency_score",
      rideRequestPopupDelayDuration = "ride_request_popup_delay_duration",
      searchRequestValidTill = "search_request_valid_till",
      searchTryId = "search_try_id",
      startTime = "start_time",
      status = "status",
      straightLineDistanceToPickup = "straight_line_distance_to_pickup",
      totalRides = "total_rides",
      updatedAt = "updated_at",
      vehicleAge = "vehicle_age",
      vehicleServiceTier = "vehicle_service_tier",
      vehicleServiceTierName = "vehicle_service_tier_name",
      vehicleVariant = "vehicle_variant"
    }

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(TH.mkClickhouseInstances ''SearchRequestForDriverT 'SELECT_FINAL_MODIFIER)

calulateSupplyDemandByGeohashAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Int, DServiceTierType.ServiceTierType)]
calulateSupplyDemandByGeohashAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let supplyCount = CH.count_ (CH.distinct srfd.driverId)
          let demandCount = CH.count_ (CH.distinct srfd.requestId)
          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleServiceTier) $ \(fromLocGeohash, vehicleServiceTier) -> do
            (fromLocGeohash, supplyCount, demandCount, vehicleServiceTier)
      )
      $ CH.filter_
        ( \srfd _ ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. srfd.mode CH.==. Just DI.ONLINE
              CH.&&. CH.isNotNull srfd.fromLocGeohash
              --   CH.&&. CH.not_ (srfd.vehicleServiceTier `in_` [DServiceTierType.AUTO_RICKSHAW, DServiceTierType.BIKE, DServiceTierType.AMBULANCE_TAXI, DServiceTierType.AMBULANCE_TAXI_OXY, DServiceTierType.AMBULANCE_AC, DServiceTierType.AMBULANCE_AC_OXY, DServiceTierType.AMBULANCE_VENTILATOR, DServiceTierType.DELIVERY_BIKE])
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

findByDriverForLastXMinute ::
  (CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, MonadFlow m) =>
  Id DP.Person ->
  Int ->
  m [SearchRequestForDriver]
findByDriverForLastXMinute driverId xMin = do
  now <- getCurrentTime
  let startTime = T.addUTCTime (-60 * fromIntegral xMin) now
  CH.findAll $
    CH.select $
      CH.orderBy_ (\srfd _ -> CH.desc $ srfd.createdAt) $
        CH.filter_
          ( \srfd _ ->
              srfd.driverId CH.==. driverId
                CH.&&. srfd.createdAt >=. CH.DateTime startTime
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)
