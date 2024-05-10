{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequestForDriver where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import SharedLogic.DriverPool.Types
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SearchRequest as QR

searchReqestForDriverkey :: Text -> Text
searchReqestForDriverkey prefix = "searchRequestForDriver_" <> prefix

searchReqestForDriverkeyExpiry :: Integer
searchReqestForDriverkeyExpiry = 5 * 60 -- 5 mins

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => SearchRequestForDriver -> m ()
    createOne srd = do
      now <- getCurrentTime
      when (srd.status == Domain.Active) $ do
        let driverId = getId $ Domain.driverId srd
        clearOldSrfdIds driverId
        void $
          Hedis.withCrossAppRedis $ Hedis.zAddExp (searchReqestForDriverkey driverId) (getId $ Domain.id srd) (round $ utcToMilliseconds now) (fromInteger searchReqestForDriverkeyExpiry)
      createWithKV srd

    clearOldSrfdIds :: (MonadFlow m, HedisFlow m r) => Text -> m ()
    clearOldSrfdIds driverId = do
      now <- getCurrentTime
      let startTime = T.addUTCTime (-1 * (fromIntegral $ searchReqestForDriverkeyExpiry + 1)) now -- sorted set key would expire beyond this anyways
      let endTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
      void $ Hedis.withCrossAppRedis $ Hedis.zRemRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds endTime)

findAllActiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveBySTId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findAllActiveBySRId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId (Id searchReqId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.requestId $ Se.Eq searchReqId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findByDriverAndSearchTryId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) =
  findOneWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
        )
    ]

findByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  let startTime = T.addUTCTime (-60) now -- conservative 1min lookback time to accomodate different configs for SearchRequestForDriver expiry
  srfdIds <-
    Hedis.withCrossAppRedis $ Hedis.zRangeByScore (searchReqestForDriverkey driverId) (utcToMilliseconds startTime) (utcToMilliseconds now)
  findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.id $ Se.In (map TE.decodeUtf8 srfdIds), Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id Person -> m ()
deleteByDriverId (Id personId) = do
  void $ Hedis.del (searchReqestForDriverkey personId)
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]

setInactiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HedisFlow m r) => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = do
  srfds <- findAllWithKV [Se.And [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]) srfds -- this will remove the key from redis
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveAndPulledByIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id SearchRequestForDriver] -> m ()
setInactiveAndPulledByIds srdIds = do
  srfd <- findAllWithKV [Se.And [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ Hedis.withCrossAppRedis $ Hedis.zRem (searchReqestForDriverkey $ getId $ Domain.driverId s) [getId $ Domain.id s]) srfd -- this will remove the key from redis
  updateWithKV
    [ Se.Set BeamSRFD.status Domain.Inactive,
      Se.Set BeamSRFD.response (Just Domain.Pulled)
    ]
    [Se.Is BeamSRFD.id (Se.In $ (.getId) <$> srdIds)]

updateDriverResponse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> DriverSearchRequestStatus -> m ()
updateDriverResponse (Id id) response status =
  updateOneWithKV
    [ Se.Set BeamSRFD.response (Just response),
      Se.Set BeamSRFD.status status
    ]
    [Se.Is BeamSRFD.id (Se.Eq id)]

instance FromTType' BeamSRFD.SearchRequestForDriver SearchRequestForDriver where
  fromTType' BeamSRFD.SearchRequestForDriverT {..} = do
    merchant <- case merchantId of
      Nothing -> do
        searchReq <- QR.findById (Id requestId) >>= fromMaybeM (InternalError $ "Search request not found : " <> requestId)
        CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
      Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
    clientSdkVersion' <- mapM readVersion (T.strip <$> clientSdkVersion)
    clientBundleVersion' <- mapM readVersion (T.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM readVersion (T.strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM readVersion (T.strip <$> backendConfigVersion)
    let clientDevice' = mkClientDevice clientOsType clientOsVersion
    pure $
      Just
        SearchRequestForDriver
          { id = Id id,
            requestId = Id requestId,
            searchTryId = Id searchTryId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = merchantOpCityId,
            searchRequestValidTill = T.localTimeToUTC T.utc searchRequestValidTill,
            driverId = Id driverId,
            createdAt = T.localTimeToUTC T.utc createdAt,
            vehicleServiceTier = fromMaybe (castVariantToServiceTier vehicleVariant) vehicleServiceTier,
            goHomeRequestId = Id <$> goHomeRequestId,
            clientSdkVersion = clientSdkVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            backendConfigVersion = backendConfigVersion',
            clientDevice = clientDevice',
            baseFare = mkAmountWithDefault baseFareAmount <$> baseFare,
            driverMinExtraFee = mkAmountWithDefault driverMinExtraFeeAmount <$> driverMinExtraFee,
            driverMaxExtraFee = mkAmountWithDefault driverMaxExtraFeeAmount <$> driverMaxExtraFee,
            customerCancellationDues = fromMaybe 0.0 customerCancellationDues,
            driverStepFee = mkAmountWithDefault driverStepFeeAmount <$> driverStepFee,
            driverDefaultStepFee = mkAmountWithDefault driverDefaultStepFeeAmount <$> driverDefaultStepFee,
            currency = fromMaybe INR currency,
            ..
          }

instance ToTType' BeamSRFD.SearchRequestForDriver SearchRequestForDriver where
  toTType' SearchRequestForDriver {..} = do
    BeamSRFD.SearchRequestForDriverT
      { BeamSRFD.id = getId id,
        BeamSRFD.requestId = getId requestId,
        BeamSRFD.searchTryId = getId searchTryId,
        BeamSRFD.estimateId = estimateId,
        BeamSRFD.baseFare = roundToIntegral <$> baseFare,
        BeamSRFD.baseFareAmount = baseFare,
        BeamSRFD.merchantId = getId <$> merchantId,
        BeamSRFD.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamSRFD.startTime = startTime,
        BeamSRFD.searchRequestValidTill = T.utcToLocalTime T.utc searchRequestValidTill,
        BeamSRFD.driverId = getId driverId,
        BeamSRFD.actualDistanceToPickup = actualDistanceToPickup,
        BeamSRFD.straightLineDistanceToPickup = straightLineDistanceToPickup,
        BeamSRFD.durationToPickup = durationToPickup,
        BeamSRFD.vehicleVariant = vehicleVariant,
        BeamSRFD.vehicleServiceTier = Just vehicleServiceTier,
        BeamSRFD.vehicleServiceTierName = vehicleServiceTierName,
        BeamSRFD.airConditioned = airConditioned,
        BeamSRFD.status = status,
        BeamSRFD.batchNumber = batchNumber,
        BeamSRFD.lat = lat,
        BeamSRFD.lon = lon,
        BeamSRFD.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamSRFD.response = response,
        BeamSRFD.driverMinExtraFee = roundToIntegral <$> driverMinExtraFee,
        BeamSRFD.driverMaxExtraFee = roundToIntegral <$> driverMaxExtraFee,
        BeamSRFD.driverMinExtraFeeAmount = driverMinExtraFee,
        BeamSRFD.driverMaxExtraFeeAmount = driverMaxExtraFee,
        BeamSRFD.driverStepFee = roundToIntegral <$> driverStepFee,
        BeamSRFD.driverDefaultStepFee = roundToIntegral <$> driverDefaultStepFee,
        BeamSRFD.driverStepFeeAmount = driverStepFee,
        BeamSRFD.driverDefaultStepFeeAmount = driverDefaultStepFee,
        BeamSRFD.currency = Just currency,
        BeamSRFD.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
        BeamSRFD.isPartOfIntelligentPool = isPartOfIntelligentPool,
        BeamSRFD.pickupZone = pickupZone,
        BeamSRFD.cancellationRatio = cancellationRatio,
        BeamSRFD.acceptanceRatio = acceptanceRatio,
        BeamSRFD.driverAvailableTime = driverAvailableTime,
        BeamSRFD.parallelSearchRequestCount = parallelSearchRequestCount,
        BeamSRFD.keepHiddenForSeconds = keepHiddenForSeconds,
        BeamSRFD.driverSpeed = driverSpeed,
        BeamSRFD.mode = mode,
        BeamSRFD.goHomeRequestId = getId <$> goHomeRequestId,
        BeamSRFD.rideFrequencyScore = rideFrequencyScore,
        BeamSRFD.customerCancellationDues = Just customerCancellationDues,
        BeamSRFD.clientSdkVersion = versionToText <$> clientSdkVersion,
        BeamSRFD.clientBundleVersion = versionToText <$> clientBundleVersion,
        BeamSRFD.clientConfigVersion = versionToText <$> clientConfigVersion,
        BeamSRFD.backendConfigVersion = versionToText <$> backendConfigVersion,
        BeamSRFD.clientOsVersion = clientDevice <&> (.deviceVersion),
        BeamSRFD.clientOsType = clientDevice <&> (.deviceType),
        ..
      }
