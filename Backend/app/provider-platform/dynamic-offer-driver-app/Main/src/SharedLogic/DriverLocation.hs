{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverLocation where

import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow, EsqLocDBFlow, EsqLocRepDBFlow)
-- import Kernel.Storage.Esqueleto.Transactionable (runInLocationDB)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.Queries.DriverLocation as DLQueries

upsertGpsCoord :: (CacheFlow m r, L.MonadFlow m, MonadTime m, EsqLocDBFlow m r, EsqLocRepDBFlow m r) => Id Person -> LatLong -> UTCTime -> Id Merchant -> m ()
upsertGpsCoord driverId latLong calculationTime merchantId = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  if not driverInfo.onRide -- if driver not on ride directly save location updates to DB
  -- then void $ runInLocationDB $ DLQueries.upsertGpsCoord driverId latLong calculationTime merchantId
    then void $ DLQueries.upsertGpsCoord driverId latLong calculationTime merchantId
    else do
      mOldLocation <- findById merchantId driverId
      case mOldLocation of
        Nothing -> do
          -- driverLocation <- runInLocationDB $ DLQueries.upsertGpsCoord driverId latLong calculationTime merchantId
          driverLocation <- DLQueries.upsertGpsCoord driverId latLong calculationTime merchantId
          cacheDriverLocation driverLocation
        Just oldLoc -> do
          now <- getCurrentTime
          cacheDriverLocation $ oldLoc{updatedAt = now, lat = latLong.lat, lon = latLong.lon, coordinatesCalculatedAt = calculationTime}

makeDriverLocationKey :: Id Person -> Text
makeDriverLocationKey id = "DriverLocation:PersonId-" <> id.getId

findById :: (CacheFlow m r, L.MonadFlow m, MonadTime m, EsqLocRepDBFlow m r) => Id Merchant -> Id Person -> m (Maybe DriverLocation)
findById merchantId id =
  Hedis.safeGet (makeDriverLocationKey id) >>= \case
    Just a ->
      return $ Just a
    Nothing -> do
      location <- getDriverLocationWithoutMerchantId id
      case location of
        Just a ->
          return $ Just $ mkDriverLocation a merchantId
        Nothing ->
          -- flip whenJust cacheDriverLocation /=<< DLQueries.findByIdInReplica id
          flip whenJust cacheDriverLocation /=<< DLQueries.findById id

getDriverLocationWithoutMerchantId :: (CacheFlow m r, L.MonadFlow m) => Id Person -> m (Maybe DriverLocationWithoutMerchantId)
getDriverLocationWithoutMerchantId = Hedis.safeGet . makeDriverLocationKey

mkDriverLocation :: DriverLocationWithoutMerchantId -> Id Merchant -> DriverLocation
mkDriverLocation DriverLocationWithoutMerchantId {..} merchantId =
  DriverLocation
    { merchantId = merchantId,
      ..
    }

-- TODO: REMOVE DriverLocationWithoutMerchantId AS DONE FOR BACKWARD COMPATIBILITY
data DriverLocationWithoutMerchantId = DriverLocationWithoutMerchantId
  { driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    coordinatesCalculatedAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

cacheDriverLocation :: (CacheFlow m r) => DriverLocation -> m ()
cacheDriverLocation driverLocation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let driverLocationKey = makeDriverLocationKey driverLocation.driverId
  Hedis.setExp driverLocationKey driverLocation expTime

updateOnRide :: (CacheFlow m r, L.MonadFlow m, MonadTime m, EsqLocDBFlow m r, EsqLocRepDBFlow m r) => Id Person.Driver -> Bool -> Id Merchant -> m (MeshResult ())
updateOnRide driverId onRide merchantId = do
  if onRide
    then do
      -- driver coming to ride, update location from db to redis
      driverLocation <- DLQueries.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
      cacheDriverLocation driverLocation
    else do
      -- driver going out of ride, update location from redis to db
      mDriverLocatation <- findById merchantId (cast driverId)
      maybe
        (pure ())
        ( \loc -> do
            let latLong = LatLong loc.lat loc.lon
            -- void $ Esq.runInLocationDB $ DLQueries.upsertGpsCoord (cast driverId) latLong loc.coordinatesCalculatedAt merchantId
            void $ DLQueries.upsertGpsCoord (cast driverId) latLong loc.coordinatesCalculatedAt merchantId
        )
        mDriverLocatation
  CDI.updateOnRide driverId onRide

updateOnRideCache :: (CacheFlow m r, Esq.EsqDBFlow m r, EsqLocDBFlow m r) => Id Person.Driver -> m ()
updateOnRideCache driverId = do
  driverLocation <- DLQueries.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  cacheDriverLocation driverLocation
  CDI.clearDriverInfoCache driverId

updateOnRideCacheForCancelledOrEndRide :: (CacheFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r, EsqLocDBFlow m r, EsqLocRepDBFlow m r) => Id Person.Driver -> Id Merchant -> m ()
updateOnRideCacheForCancelledOrEndRide driverId merchantId = do
  mbDriverLocation <- findById merchantId (cast driverId)
  maybe
    (pure ())
    ( \loc -> do
        let latLong = LatLong loc.lat loc.lon
        -- void $ Esq.runInLocationDB $ DLQueries.upsertGpsCoord (cast driverId) latLong loc.coordinatesCalculatedAt merchantId
        void $ DLQueries.upsertGpsCoord (cast driverId) latLong loc.coordinatesCalculatedAt merchantId
    )
    mbDriverLocation
  CDI.clearDriverInfoCache driverId
