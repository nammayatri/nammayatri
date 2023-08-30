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

import qualified Data.Text.Encoding as TE
import qualified Data.Time as T
import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD

createMany :: MonadFlow m => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: MonadFlow m => SearchRequestForDriver -> m ()
    createOne srd = do
      when (srd.status == Domain.Active) $ do
        void $
          L.runKVDB meshConfig.kvRedis $
            L.sadd (TE.encodeUtf8 (BeamSRFD.searchReqestForDriverkey $ getId $ Domain.driverId srd)) [TE.encodeUtf8 (getId $ Domain.id srd)]
      createWithKV srd

findAllActiveBySTId :: MonadFlow m => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveBySTId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findAllActiveBySRId :: MonadFlow m => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId (Id searchReqId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.requestId $ Se.Eq searchReqId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findAllActiveWithoutRespBySearchTryId :: MonadFlow m => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchTryId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.response $ Se.Eq Nothing]
        )
    ]

findByDriverAndSearchTryId :: MonadFlow m => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) =
  findOneWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
        )
    ]

-- Should not support driver Id as a secondryKey index in Kv so creating a new key while creating the entry in redis for active searchRequestForDriver
findByDriver :: MonadFlow m => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  srfdIds <-
    L.runKVDB meshConfig.kvRedis $
      L.smembers (TE.encodeUtf8 $ BeamSRFD.searchReqestForDriverkey driverId)
  findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.id $ Se.In (map TE.decodeUtf8 $ BeamSRFD.extractValue srfdIds), Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

deleteByDriverId :: MonadFlow m => Id Person -> m ()
deleteByDriverId (Id personId) = do
  void $ L.runKVDB meshConfig.kvRedis $ L.del [TE.encodeUtf8 $ BeamSRFD.searchReqestForDriverkey personId]
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]

setInactiveBySTId :: MonadFlow m => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = do
  srfds <- findAllWithKV [Se.And [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ L.runKVDB meshConfig.kvRedis $ L.srem (TE.encodeUtf8 $ BeamSRFD.searchReqestForDriverkey $ getId $ Domain.driverId s) [TE.encodeUtf8 $ getId $ Domain.id s]) srfds -- this will remove the key from redis
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveBySRId :: MonadFlow m => Id SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = do
  srfd <- findAllWithKV [Se.And [Se.Is BeamSRFD.requestId (Se.Eq searchReqId), Se.Is BeamSRFD.status (Se.Eq Domain.Active)]]
  mapM_ (\s -> void $ L.runKVDB meshConfig.kvRedis $ L.srem (TE.encodeUtf8 $ BeamSRFD.searchReqestForDriverkey $ getId $ Domain.driverId s) [TE.encodeUtf8 $ getId $ Domain.id s]) srfd -- this will remove the key from redis
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.requestId (Se.Eq searchReqId)]

updateDriverResponse :: MonadFlow m => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> m ()
updateDriverResponse (Id id) response =
  updateOneWithKV
    [Se.Set BeamSRFD.response (Just response)]
    [Se.Is BeamSRFD.id (Se.Eq id)]

instance FromTType' BeamSRFD.SearchRequestForDriver SearchRequestForDriver where
  fromTType' BeamSRFD.SearchRequestForDriverT {..} = do
    pure $
      Just
        SearchRequestForDriver
          { id = Id id,
            requestId = Id requestId,
            searchTryId = Id searchTryId,
            merchantId = Id <$> merchantId,
            startTime = startTime,
            searchRequestValidTill = T.localTimeToUTC T.utc searchRequestValidTill,
            driverId = Id driverId,
            actualDistanceToPickup = actualDistanceToPickup,
            straightLineDistanceToPickup = straightLineDistanceToPickup,
            durationToPickup = durationToPickup,
            vehicleVariant = vehicleVariant,
            status = status,
            batchNumber = batchNumber,
            lat = lat,
            lon = lon,
            createdAt = T.localTimeToUTC T.utc createdAt,
            response = response,
            driverMinExtraFee = driverMinExtraFee,
            driverMaxExtraFee = driverMaxExtraFee,
            rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
            isPartOfIntelligentPool = isPartOfIntelligentPool,
            cancellationRatio = cancellationRatio,
            acceptanceRatio = acceptanceRatio,
            driverAvailableTime = driverAvailableTime,
            parallelSearchRequestCount = parallelSearchRequestCount,
            keepHiddenForSeconds = keepHiddenForSeconds,
            driverSpeed = driverSpeed,
            mode = mode,
            goHomeRequestId = Id <$> goHomeRequestId
          }

instance ToTType' BeamSRFD.SearchRequestForDriver SearchRequestForDriver where
  toTType' SearchRequestForDriver {..} = do
    BeamSRFD.SearchRequestForDriverT
      { BeamSRFD.id = getId id,
        BeamSRFD.requestId = getId requestId,
        BeamSRFD.searchTryId = getId searchTryId,
        BeamSRFD.merchantId = getId <$> merchantId,
        BeamSRFD.startTime = startTime,
        BeamSRFD.searchRequestValidTill = T.utcToLocalTime T.utc searchRequestValidTill,
        BeamSRFD.driverId = getId driverId,
        BeamSRFD.actualDistanceToPickup = actualDistanceToPickup,
        BeamSRFD.straightLineDistanceToPickup = straightLineDistanceToPickup,
        BeamSRFD.durationToPickup = durationToPickup,
        BeamSRFD.vehicleVariant = vehicleVariant,
        BeamSRFD.status = status,
        BeamSRFD.batchNumber = batchNumber,
        BeamSRFD.lat = lat,
        BeamSRFD.lon = lon,
        BeamSRFD.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamSRFD.response = response,
        BeamSRFD.driverMinExtraFee = driverMinExtraFee,
        BeamSRFD.driverMaxExtraFee = driverMaxExtraFee,
        BeamSRFD.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
        BeamSRFD.isPartOfIntelligentPool = isPartOfIntelligentPool,
        BeamSRFD.cancellationRatio = cancellationRatio,
        BeamSRFD.acceptanceRatio = acceptanceRatio,
        BeamSRFD.driverAvailableTime = driverAvailableTime,
        BeamSRFD.parallelSearchRequestCount = parallelSearchRequestCount,
        BeamSRFD.keepHiddenForSeconds = keepHiddenForSeconds,
        BeamSRFD.driverSpeed = driverSpeed,
        BeamSRFD.mode = mode,
        BeamSRFD.goHomeRequestId = getId <$> goHomeRequestId
      }
