{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverLocation where

import Data.Either
import Data.Maybe as Mb
import Data.Time (addUTCTime)
import qualified Database.Beam as B
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgExpressionSyntax (..), emit)
import qualified Database.Beam.Query as BQ
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error
import Lib.Utils (buildRadiusWithin'')
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverLocation as BeamDL
import Tools.Error

create :: MonadFlow m => Id Person -> LatLong -> UTCTime -> Id Merchant -> m ()
create drLocationId latLong updateTime merchantId = do
  now <- getCurrentTime
  dbConf <- getLocationDbBeamConfig
  void $ L.runDB dbConf $ L.insertRows $ B.insert (BeamCommon.driverLocation BeamCommon.atlasDB) $ B.insertExpressions [BeamDL.toRowExpression (getId drLocationId) latLong updateTime now (getId merchantId)]

findById :: (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) => Id Person -> m (Maybe DriverLocation)
findById (Id driverLocationId) = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  dbConf <- getLocationDbBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.limit_ 1 $
            B.filter_' (\BeamDL.DriverLocationT {..} -> driverId B.==?. B.val_ driverLocationId) $
              B.all_ (BeamCommon.driverLocation BeamCommon.atlasDB)
  case geoms of
    Right (Just geom) -> fromTType' geom
    _ -> return Nothing

upsertGpsCoord :: (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) => Id Person -> LatLong -> UTCTime -> Id Merchant -> m DriverLocation
upsertGpsCoord drLocationId latLong calculationTime merchantId' = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  now <- getCurrentTime
  res <- findById drLocationId
  case res of
    Just _ -> updateRecords (getId drLocationId) latLong calculationTime now
    Nothing -> create drLocationId latLong calculationTime merchantId'
  let updatedRecord =
        DriverLocation
          { driverId = drLocationId,
            lat = latLong.lat,
            lon = latLong.lon,
            coordinatesCalculatedAt = calculationTime,
            createdAt = now,
            updatedAt = now,
            merchantId = merchantId'
          }
  return updatedRecord
  where
    updateRecords :: MonadFlow m => Text -> LatLong -> UTCTime -> UTCTime -> m ()
    updateRecords drLocationId' latLong' calculationTime' now' = do
      dbConf <- getLocationDbBeamConfig
      void $
        L.runDB dbConf $
          L.updateRows $
            B.update
              (BeamCommon.driverLocation BeamCommon.atlasDB)
              ( \BeamDL.DriverLocationT {..} ->
                  (driverId B.<-. B.val_ drLocationId') <> (lat B.<-. B.val_ latLong'.lat)
                    <> (lon B.<-. (B.val_ latLong'.lon))
                    <> (coordinatesCalculatedAt B.<-. B.val_ calculationTime')
                    <> (point B.<-. getPoint (latLong'.lat, latLong'.lon))
                    <> (updatedAt B.<-. B.val_ now')
              )
              (\BeamDL.DriverLocationT {..} -> driverId B.==. B.val_ drLocationId')

deleteById :: MonadFlow m => Id Person -> m ()
deleteById (Id driverId') = do
  dbConf <- getLocationDbBeamConfig
  void $
    L.runDB dbConf $
      L.deleteRows
        ( B.delete
            (BeamCommon.driverLocation BeamCommon.atlasDB)
            (\BeamDL.DriverLocationT {..} -> driverId B.==. B.val_ driverId')
        )

getDriverLocsFromMerchId ::
  (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) =>
  Maybe Seconds ->
  LatLong ->
  Int ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocsFromMerchId mbDriverPositionInfoExpiry gps radiusMeters merchantId' = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  let expTime = maybe 0 getSeconds mbDriverPositionInfoExpiry
  now <- getCurrentTime
  dbConf <- getLocationDbBeamConfig
  dlocs <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.orderBy_ (\_ -> B.asc_ (byDist (gps.lat, gps.lon))) $
            B.filter_'
              ( \BeamDL.DriverLocationT {..} ->
                  merchantId B.==?. B.val_ (getId merchantId')
                    B.&&?. (B.sqlBool_ (B.val_ (Mb.isNothing mbDriverPositionInfoExpiry)) B.||?. B.sqlBool_ (coordinatesCalculatedAt B.>=. B.val_ (addUTCTime (fromIntegral (-1 * expTime)) now)))
                    B.&&?. buildRadiusWithin'' (gps.lat, gps.lon) radiusMeters
              )
              $ B.all_ (BeamCommon.driverLocation BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] dlocs)

byDist :: (Double, Double) -> BQ.QGenExpr context Postgres s Double
byDist (lat, lon) = BQ.QExpr (\_ -> PgExpressionSyntax (emit $ "point <-> 'SRID=4326;POINT(" <> show lon <> " " <> show lat <> ")'"))

findAllDriverLocations ::
  (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) =>
  [Id Person] ->
  Maybe Seconds ->
  UTCTime ->
  m [DriverLocation]
findAllDriverLocations driverIds mbDriverPositionInfoExpiry now = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  let expTime = maybe 0 getSeconds mbDriverPositionInfoExpiry
  dbConf <- getLocationDbBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamDL.DriverLocationT {..} ->
                (B.sqlBool_ (B.val_ (Mb.isNothing mbDriverPositionInfoExpiry)) B.||?. B.sqlBool_ (coordinatesCalculatedAt B.>=. B.val_ (addUTCTime (fromIntegral (-1 * expTime)) now))) B.&&?. B.sqlBool_ (driverId `B.in_` (B.val_ . getId <$> driverIds))
            )
            $ B.all_ (BeamCommon.driverLocation BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

getDriverLocations ::
  (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) =>
  UTCTime ->
  m [DriverLocation]
getDriverLocations before = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  dbConf <- getLocationDbBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamDL.DriverLocationT {..} ->
                B.sqlBool_ (updatedAt B.<. B.val_ before)
            )
            $ B.all_ (BeamCommon.driverLocation BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

getDriverLocs ::
  (MonadFlow m, MonadReader r m, HasField "enableLocationTrackingService" r Bool) =>
  [Id Person] ->
  Id Merchant ->
  m [DriverLocation]
getDriverLocs driverIds (Id merchId) = do
  enableLocationTrackingService <- asks (.enableLocationTrackingService)
  when enableLocationTrackingService $ do
    throwError InvalidLocationTrackingException
  dbConf <- getLocationDbBeamConfig
  geoms <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.filter_'
            ( \BeamDL.DriverLocationT {..} ->
                B.sqlBool_ (driverId `B.in_` (B.val_ . getId <$> driverIds))
                  B.&&?. B.sqlBool_ (merchantId B.==. B.val_ merchId)
            )
            $ B.all_ (BeamCommon.driverLocation BeamCommon.atlasDB)
  catMaybes <$> mapM fromTType' (fromRight [] geoms)

instance FromTType' BeamDL.DriverLocation DriverLocation where
  fromTType' BeamDL.DriverLocationT {..} = do
    pure $
      Just
        DriverLocation
          { driverId = Id driverId,
            lat = lat,
            lon = lon,
            coordinatesCalculatedAt = coordinatesCalculatedAt,
            createdAt = createdAt,
            updatedAt = updatedAt,
            merchantId = Id merchantId
          }
