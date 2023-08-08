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
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD

createMany :: (L.MonadFlow m, Log m) => [SearchRequestForDriver] -> m ()
createMany = traverse_ createOne
  where
    createOne :: (L.MonadFlow m, Log m) => SearchRequestForDriver -> m ()
    createOne = createWithKV

findAllActiveBySTId :: (L.MonadFlow m, Log m) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveBySTId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findAllActiveBySRId :: (L.MonadFlow m, Log m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId (Id searchReqId) =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamSRFD.requestId $ Se.Eq searchReqId,
          Se.Is BeamSRFD.status $ Se.Eq Domain.Active
        ]
    ]

findAllActiveWithoutRespBySearchTryId :: (L.MonadFlow m, MonadTime m, Log m) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchTryId (Id searchTryId) =
  findAllWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.response $ Se.Eq Nothing]
        )
    ]

findByDriverAndSearchTryId :: (L.MonadFlow m, Log m) => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId (Id driverId) (Id searchTryId) =
  findOneWithKV
    [ Se.And
        ( [Se.Is BeamSRFD.searchTryId $ Se.Eq searchTryId]
            <> [Se.Is BeamSRFD.status $ Se.Eq Domain.Active]
            <> [Se.Is BeamSRFD.driverId $ Se.Eq driverId]
        )
    ]

-- Should not support driver Id as a key in index, since it will be 1 to many; routing these queries through DB
findByDriver :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamSRFD.driverId $ Se.Eq driverId, Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan (T.utcToLocalTime T.utc now)]] (Se.Desc BeamSRFD.searchRequestValidTill) Nothing Nothing

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id personId) =
  deleteWithKV
    [Se.Is BeamSRFD.driverId (Se.Eq personId)]

setInactiveBySTId :: (L.MonadFlow m, Log m) => Id SearchTry -> m ()
setInactiveBySTId (Id searchTryId) =
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]

setInactiveBySRId :: (L.MonadFlow m, Log m) => Id SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) =
  updateWithKV
    [Se.Set BeamSRFD.status Domain.Inactive]
    [Se.Is BeamSRFD.requestId (Se.Eq searchReqId)]

updateDriverResponse :: (L.MonadFlow m, Log m) => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> m ()
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
            mode = mode
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
        BeamSRFD.mode = mode
      }
