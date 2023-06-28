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
-- import qualified Database.Beam.Query as B

-- import qualified EulerHS.KVConnector.Flow as KV
-- import EulerHS.KVConnector.Types

import qualified Database.Beam.Schema.Tables as B
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person
-- import qualified EulerHS.KVConnector.Flow as KV
-- import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Lib.Utils
import qualified Storage.Beam.DriverLocation as BeamDL

data AtlasDB f = AtlasDB
  { driverLocation :: f (B.TableEntity BeamDL.DriverLocationT)
  }
  deriving (Generic, B.Database be)

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { driverLocation = dLocationTable
      }

dLocationTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BeamDL.DriverLocationT)
dLocationTable =
  B.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "driver_location"
    <> B.modifyTableFields BeamDL.driverLocationTMod

-- driverLocationEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity BeamDL.DriverLocationT)
-- driverLocationEMod = B.modifyTableFields BeamDL.driverLocationTMod

create :: (L.MonadFlow m, MonadTime m) => Id Person -> LatLong -> UTCTime -> Id Merchant -> m ()
create drLocationId latLong updateTime merchantId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  now <- getCurrentTime
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      let
      -- void $ L.runDB c $ L.insertRows $ B.insert (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT)) $ B.insertExpressions [BeamDL.toRowExpression (getId drLocationId) latLong updateTime now (getId merchantId)]
      void $ L.runDB c $ L.insertRows $ B.insert (driverLocation atlasDB) $ B.insertExpressions [BeamDL.toRowExpression (getId drLocationId) latLong updateTime now (getId merchantId)]
    Left _ -> pure ()

findById :: L.MonadFlow m => Id Person -> m (Maybe DriverLocation)
findById (Id driverLocationId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      geoms <-
        L.runDB c $
          L.findRow $
            B.select $
              B.limit_ 1 $
                B.filter_' (\BeamDL.DriverLocationT {..} -> driverId B.==?. B.val_ driverLocationId) $
                  -- B.all_ (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                  B.all_ (driverLocation atlasDB)
      pure (either (const Nothing) (transformBeamDriverLocationToDomain <$>) geoms)
    Left _ -> pure Nothing

findByIdInReplica ::
  (Transactionable m, EsqLocRepDBFlow m r) =>
  Id Person ->
  m (Maybe DriverLocation)
findByIdInReplica id = runInLocReplica $ Esq.findById id

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
      dbConf <- L.getOption KBT.PsqlDbCfg
      conn <- L.getOrInitSqlConn (fromJust dbConf)
      case conn of
        Right c -> do
          void $
            L.runDB c $
              L.updateRows $
                B.update
                  -- (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                  (driverLocation atlasDB)
                  ( \BeamDL.DriverLocationT {..} ->
                      (driverId B.<-. B.val_ drLocationId') <> (lat B.<-. B.val_ latLong'.lat)
                        <> (lon B.<-. (B.val_ latLong'.lon))
                        <> (coordinatesCalculatedAt B.<-. B.val_ calculationTime')
                        <> (point B.<-. getPoint (latLong'.lat, latLong'.lon))
                        <> (updatedAt B.<-. B.val_ now')
                  )
                  (\BeamDL.DriverLocationT {..} -> driverId B.==. B.val_ drLocationId')
        Left _ -> pure ()

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id driverId') = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      void $
        L.runDB c $
          L.deleteRows
            ( B.delete
                -- (meshModelTableEntity @BeamDL.DriverLocationT @Postgres @(Se.DatabaseWith BeamDL.DriverLocationT))
                (driverLocation atlasDB)
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
