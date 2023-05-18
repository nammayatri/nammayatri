{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequestForDriver where

import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.SearchRequestForDriver as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import Storage.Tabular.SearchRequestForDriver
import qualified Storage.Tabular.VechileNew as VN

createMany :: [SearchRequestForDriver] -> SqlDB ()
createMany = Esq.createMany

findAllActiveBySRId :: (Transactionable m, MonadTime m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId searchReqId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    pure sReq

findAllActiveWithoutRespByRequestId :: (Transactionable m, MonadTime m) => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveWithoutRespByRequestId searchReqId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
        &&. Esq.isNothing (sReq ^. SearchRequestForDriverResponse)
    pure sReq

findByDriverAndSearchReq :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchReq driverId searchReqId = Esq.findOne $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)
      &&. sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
      &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
  pure sReq

findByDriver :: (Transactionable m, MonadTime m) => Id Person -> m [SearchRequestForDriver]
findByDriver driverId = do
  now <- getCurrentTime
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
        &&. sReq ^. SearchRequestForDriverSearchRequestValidTill >. val now
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
    orderBy [desc $ sReq ^. SearchRequestForDriverSearchRequestValidTill]
    pure sReq

removeAllBySearchId :: Id SearchRequest -> SqlDB ()
removeAllBySearchId searchReqId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId personId = Esq.delete $ do
  sReqForDriver <- from $ table @SearchRequestForDriverT
  where_ $ sReqForDriver ^. SearchRequestForDriverDriverId ==. val (toKey personId)

setInactiveBySRId :: Id SearchRequest -> SqlDB ()
setInactiveBySRId searchReqId = Esq.update $ \p -> do
  set p [SearchRequestForDriverStatus =. val Domain.Inactive]
  where_ $ p ^. SearchRequestForDriverSearchRequestId ==. val (toKey searchReqId)

updateDriverResponse :: Id SearchRequestForDriver -> SearchRequestForDriverResponse -> SqlDB ()
updateDriverResponse id response = Esq.update $ \p -> do
  set p [SearchRequestForDriverResponse =. val (Just response)]
  where_ $ p ^. SearchRequestForDriverId ==. val (getId id)

-- transformBeamSearchRequestForDriverToDomain :: BeamSRFD.SearchRequestForDriver -> SearchRequestForDriver
-- transformBeamSearchRequestForDriverToDomain BeamSRFD.SearchRequestForDriverT {..} = do
--   SearchRequestForDriver
--     { id = Id id,
--       transactionId = transactionId,
--       searchRequestId = Id searchRequestId,
--       startTime = startTime,
--       searchRequestValidTill = searchRequestValidTill,
--       driverId = Id driverId,
--       actualDistanceToPickup = actualDistanceToPickup,
--       straightLineDistanceToPickup = straightLineDistanceToPickup,
--       durationToPickup = durationToPickup,
--       vehicleVariant = vehicleVariant,
--       status = status,
--       baseFare = baseFare,
--       batchNumber = batchNumber,
--       lat = lat,
--       lon = lon,
--       createdAt = createdAt,
--       response = response,
--       driverMinExtraFee = driverMinExtraFee,
--       driverMaxExtraFee = driverMaxExtraFee,
--       rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
--       isPartOfIntelligentPool = isPartOfIntelligentPool,
--       cancellationRatio = cancellationRatio,
--       acceptanceRatio = acceptanceRatio,
--       driverAvailableTime = driverAvailableTime,
--       parallelSearchRequestCount = parallelSearchRequestCount,
--       driverSpeed = driverSpeed,
--       mode = mode
--     }

-- transformDomainSearchRequestForDriverToBeam :: SearchRequestForDriver -> BeamSRFD.SearchRequestForDriver
-- transformDomainSearchRequestForDriverToBeam SearchRequestForDriver {..} =
--   BeamSRFD.defaultSearchRequestForDriver
--     { BeamSRFD.id = getId id,
--       BeamSRFD.transactionId = transactionId,
--       BeamSRFD.searchRequestId = getId searchRequestId,
--       BeamSRFD.startTime = startTime,
--       BeamSRFD.searchRequestValidTill = searchRequestValidTill,
--       BeamSRFD.driverId = getId driverId,
--       BeamSRFD.actualDistanceToPickup = actualDistanceToPickup,
--       BeamSRFD.straightLineDistanceToPickup = straightLineDistanceToPickup,
--       BeamSRFD.durationToPickup = durationToPickup,
--       BeamSRFD.vehicleVariant = vehicleVariant,
--       BeamSRFD.status = status,
--       BeamSRFD.baseFare = baseFare,
--       BeamSRFD.batchNumber = batchNumber,
--       BeamSRFD.lat = lat,
--       BeamSRFD.lon = lon,
--       BeamSRFD.createdAt = createdAt,
--       BeamSRFD.response = response,
--       BeamSRFD.driverMinExtraFee = driverMinExtraFee,
--       BeamSRFD.driverMaxExtraFee = driverMaxExtraFee,
--       BeamSRFD.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
--       BeamSRFD.isPartOfIntelligentPool = isPartOfIntelligentPool,
--       BeamSRFD.cancellationRatio = cancellationRatio,
--       BeamSRFD.acceptanceRatio = acceptanceRatio,
--       BeamSRFD.driverAvailableTime = driverAvailableTime,
--       BeamSRFD.parallelSearchRequestCount = parallelSearchRequestCount,
--       BeamSRFD.driverSpeed = driverSpeed,
--       BeamSRFD.mode = mode
--     }
