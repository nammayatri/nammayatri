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

import qualified Data.Time as T
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import SharedLogic.DriverPool.Types
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.SearchRequest as QR

createMany :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [SearchRequestForDriver] -> m ()
createMany = traverse_ createWithKV

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

findAllActiveWithoutRespBySearchTryId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchTryId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.response $ Se.Eq Nothing]
        )
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

findByDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.driverId $ Se.Eq driverId, Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

deleteByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByDriverId (Id personId) = do
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]

setInactiveBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = do
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveBySRId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = do
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.requestId (Se.Eq searchReqId)]

updateDriverResponse :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> m ()
updateDriverResponse (Id id) response =
  updateOneWithKV
    [Se.Set BeamSRFD.response (Just response)]
    [Se.Is BeamSRFD.id (Se.Eq id)]

instance FromTType' BeamSRFD.SearchRequestForDriver SearchRequestForDriver where
  fromTType' BeamSRFD.SearchRequestForDriverT {..} = do
    merchant <- case merchantId of
      Nothing -> do
        searchReq <- QR.findById (Id requestId) >>= fromMaybeM (InternalError $ "Search request not found : " <> requestId)
        CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound searchReq.providerId.getId)
      Just mId -> CQM.findById (Id mId) >>= fromMaybeM (MerchantNotFound mId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
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
            baseFare = mkAmountWithDefault baseFareAmount <$> baseFare,
            driverMinExtraFee = mkAmountWithDefault driverMinExtraFeeAmount <$> driverMinExtraFee,
            driverMaxExtraFee = mkAmountWithDefault driverMaxExtraFeeAmount <$> driverMaxExtraFee,
            customerCancellationDues = fromMaybe 0.0 customerCancellationDues,
            currency = fromMaybe INR currency, -- FIXME use default currency in migration
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
        BeamSRFD.customerCancellationDues = Just customerCancellationDues
      }
