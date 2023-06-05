{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.DriverLocation where

import qualified Database.Beam as B
import Database.Beam.Postgres
-- import qualified Database.Beam.Query as B
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person
import qualified EulerHS.Extra.EulerDB as Extra
-- import qualified EulerHS.KVConnector.Flow as KV
-- import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.DriverLocation as BeamDL

create :: (L.MonadFlow m, MonadTime m) => Id Person -> LatLong -> UTCTime -> Id Merchant -> m ()
create drLocationId latLong updateTime merchantId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      let
      void $ L.runDB c $ L.insertRows $ B.insert (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT)) $ B.insertExpressions [BeamDL.toRowExpression (getId drLocationId) latLong updateTime now (getId merchantId)]
    Left _ -> pure ()

findById :: L.MonadFlow m => Id Person -> m (Maybe DriverLocation)
findById (Id driverLocationId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      geoms <-
        L.runDB c $
          L.findRow $
            B.select $
              B.limit_ 1 $
                B.filter_' (\BeamDL.DriverLocationT {..} -> driverId B.==?. B.val_ driverLocationId) $
                  B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
      pure (either (const Nothing) (transformBeamDriverLocationToDomain <$>) geoms)
    Left _ -> pure Nothing

upsertGpsCoord :: (L.MonadFlow m, MonadTime m) => Id Person -> LatLong -> UTCTime -> Id Merchant -> m DriverLocation
upsertGpsCoord drLocationId latLong calculationTime merchantId' = do
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
    updateRecords :: L.MonadFlow m => Text -> LatLong -> UTCTime -> UTCTime -> m ()
    updateRecords drLocationId' latLong' calculationTime' now' = do
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      conn <- L.getOrInitSqlConn (fromJust dbConf)
      case conn of
        Right c -> do
          void $
            L.runDB c $
              L.updateRows $
                B.update
                  (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                  ( \BeamDL.DriverLocationT {..} ->
                      (driverId B.<-. B.val_ drLocationId') <> (lat B.<-. B.val_ latLong'.lat)
                        <> (lon B.<-. (B.val_ latLong'.lon))
                        <> (coordinatesCalculatedAt B.<-. B.val_ calculationTime')
                        <> (updatedAt B.<-. B.val_ now')
                  )
                  (\BeamDL.DriverLocationT {..} -> driverId B.==. B.val_ drLocationId')
        Left _ -> pure ()

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id driverId') = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      void $
        L.runDB c $
          L.deleteRows
            ( B.delete
                (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                (\BeamDL.DriverLocationT {..} -> driverId B.==. B.val_ driverId')
            )
    Left _ -> pure ()

transformBeamDriverLocationToDomain :: BeamDL.DriverLocation -> DriverLocation
transformBeamDriverLocationToDomain BeamDL.DriverLocationT {..} = do
  DriverLocation
    { driverId = Id driverId,
      lat = lat,
      lon = lon,
      coordinatesCalculatedAt = coordinatesCalculatedAt,
      createdAt = createdAt,
      updatedAt = updatedAt,
      merchantId = Id merchantId
    }

-- transformDomainDriverLocationToBeam :: DriverLocation -> BeamDL.DriverLocation
-- transformDomainDriverLocationToBeam DriverLocation {..} =
--   BeamDL.defaultDriverLocation
--     { BeamDL.driverId = getId driverId,
--       BeamDL.lat = lat,
--       BeamDL.lon = lon,
--       BeamDL.point = getPoint (lat, lon),
--       BeamDL.coordinatesCalculatedAt = coordinatesCalculatedAt,
--       BeamDL.createdAt = createdAt,
--       BeamDL.updatedAt = updatedAt
--     }
