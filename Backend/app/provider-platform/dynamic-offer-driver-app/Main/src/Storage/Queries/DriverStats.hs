{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverStats where

import Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverStats as BeamDS

-- import Storage.Tabular.DriverStats

-- createInitialDriverStats :: Id Driver -> SqlDB ()
-- createInitialDriverStats driverId = do
--   now <- getCurrentTime
--   Esq.create $
--     DriverStats
--       { driverId = driverId,
--         idleSince = now,
--         totalRides = 0,
--         totalDistance = 0
--       }

-- create :: DriverStats -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DriverStats -> m (MeshResult ())
create driverStats = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverStatsToBeam driverStats)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

create' :: L.MonadFlow m => DriverStats -> m (MeshResult ())
create' driverStats = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverStatsToBeam driverStats)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

createInitialDriverStats :: (L.MonadFlow m, MonadTime m) => Id Driver -> m ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  let dStats =
        DriverStats
          { driverId = driverId,
            idleSince = now,
            totalRides = 0,
            totalDistance = 0,
            ridesCancelled = Just 0,
            totalRidesAssigned = Just 0
          }
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverStatsToBeam dStats)
    Nothing -> pure ()

-- getTopDriversByIdleTime :: Transactionable m => Int -> [Id Driver] -> m [Id Driver]
-- getTopDriversByIdleTime count_ ids =
--   Esq.findAll $ do
--     driverStats <- from $ table @DriverStatsT
--     where_ $ driverStats ^. DriverStatsDriverId `in_` valList (toKey . cast <$> ids)
--     orderBy [asc $ driverStats ^. DriverStatsIdleSince]
--     limit $ fromIntegral count_
--     return $ driverStats ^. DriverStatsTId

getTopDriversByIdleTime :: L.MonadFlow m => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing
      case srsz of
        Left _ -> pure []
        Right x -> pure $ Domain.driverId . transformBeamDriverStatsToDomain <$> x
    Nothing -> pure []

-- updateIdleTime :: Id Driver -> SqlDB ()
-- updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTime :: (L.MonadFlow m, MonadTime m) => Id Driver -> m ()
updateIdleTime driverId = updateIdleTimes [driverId]

-- updateIdleTimes :: [Id Driver] -> SqlDB ()
-- updateIdleTimes driverIds = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsIdleSince =. val now
--       ]
--     where_ $ tbl ^. DriverStatsDriverId `in_` valList (toKey . cast <$> driverIds)

updateIdleTimes :: (L.MonadFlow m, MonadTime m) => [Id Driver] -> m ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          Mesh.meshConfig
          [ Se.Set BeamDS.idleSince now
          ]
          [Se.Is BeamDS.driverId (Se.In (getId <$> driverIds))]
    Nothing -> pure ()

-- fetchAll :: Transactionable m => m [DriverStats]
-- fetchAll = Esq.findAll $ from $ table @DriverStatsT

fetchAll :: L.MonadFlow m => m [DriverStats]
fetchAll = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverStatsToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig []
    Nothing -> pure []

-- findById :: Transactionable m => Id Driver -> m (Maybe DriverStats)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Driver -> m (Maybe DriverStats)
findById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverStatsToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDS.driverId $ Se.Eq driverId]
    Nothing -> pure Nothing

-- deleteById :: Id Driver -> SqlDB ()
-- deleteById = Esq.deleteByKey @DriverStatsT

deleteById :: L.MonadFlow m => Id Driver -> m ()
deleteById (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamDS.driverId (Se.Eq driverId)]
    Nothing -> pure ()

-- incrementTotalRidesAndTotalDist :: Id Driver -> Meters -> SqlDB ()
-- incrementTotalRidesAndTotalDist driverId rideDist = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsTotalRides =. (tbl ^. DriverStatsTotalRides) +. val 1,
--         DriverStatsTotalDistance =. (tbl ^. DriverStatsTotalDistance) +. val rideDist
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

incrementTotalRidesAndTotalDist :: (L.MonadFlow m) => Id Driver -> Meters -> m (MeshResult ())
incrementTotalRidesAndTotalDist (Id driverId') rideDist = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        --[ Se.Set BeamDS.totalRides $ 1,
        [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) 1,
          Se.Set BeamDS.totalDistance rideDist
        ]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- incrementTotalRidesAssigned :: Id Driver -> Int -> SqlDB ()
-- incrementTotalRidesAssigned driverId number = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsTotalRidesAssigned =. just (Esq.coalesceDefault [tbl ^. DriverStatsTotalRidesAssigned] (val 0) +. val number)
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

findTotalRidesAssigned :: (L.MonadFlow m) => Id Driver -> m (Maybe Int)
findTotalRidesAssigned (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      res <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDS.driverId $ Se.Eq driverId]
      case res of
        Left _ -> pure Nothing
        Right (Just x) -> do
          let rides = transformBeamDriverStatsToDomain x
              ans = Domain.totalRidesAssigned rides
          pure ans
        Right Nothing -> pure Nothing
    Nothing -> pure Nothing

incrementTotalRidesAssigned :: (L.MonadFlow m) => Id Driver -> Int -> m (MeshResult ())
incrementTotalRidesAssigned (Id driverId') number = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  rideAssigned <- findTotalRidesAssigned (Id driverId')
  case dbConf of
    Just dbConf' ->
      case rideAssigned of
        Just rides -> do
          let newRides = rides + number
          KV.updateWoReturningWithKVConnector
            dbConf'
            Mesh.meshConfig
            [Se.Set BeamDS.totalRidesAssigned (Just newRides)]
            [Se.Is BeamDS.driverId (Se.Eq driverId')]
        Nothing -> do
          KV.updateWoReturningWithKVConnector
            dbConf'
            Mesh.meshConfig
            [Se.Set BeamDS.totalRidesAssigned (Just number)]
            [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- setCancelledRidesCount :: Id Driver -> Int -> SqlDB ()
-- setCancelledRidesCount driverId cancelledCount = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsRidesCancelled =. val (Just cancelledCount)
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

setCancelledRidesCount :: (L.MonadFlow m) => Id Driver -> Int -> m (MeshResult ())
setCancelledRidesCount (Id driverId') cancelledCount = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [Se.Set BeamDS.ridesCancelled (Just cancelledCount)]
        [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")


-- getDriversSortedOrder :: Transactionable m => Maybe Integer -> m [DriverStats]
-- getDriversSortedOrder mbLimitVal =
--   Esq.findAll $ do
--     driverStats <- from $ table @DriverStatsT
--     orderBy [desc (driverStats ^. DriverStatsTotalRides), desc (driverStats ^. DriverStatsTotalDistance)]
--     limit $ maybe 10 fromIntegral mbLimitVal
--     return driverStats

getDriversSortedOrder :: L.MonadFlow m => Maybe Integer -> m [DriverStats]
getDriversSortedOrder mbLimitVal = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [] (Se.Desc BeamDS.totalRides) (Just $ maybe 10 fromInteger mbLimitVal) Nothing
      case srsz of
        Left _ -> pure []
        Right x -> pure $ transformBeamDriverStatsToDomain <$> x
    Nothing -> pure []

transformBeamDriverStatsToDomain :: BeamDS.DriverStats -> DriverStats
transformBeamDriverStatsToDomain BeamDS.DriverStatsT {..} = do
  DriverStats
    { driverId = Id driverId,
      idleSince = idleSince,
      totalRides = totalRides,
      totalDistance = totalDistance,
      ridesCancelled = ridesCancelled,
      totalRidesAssigned = totalRidesAssigned
    }

transformDomainDriverStatsToBeam :: DriverStats -> BeamDS.DriverStats
transformDomainDriverStatsToBeam DriverStats {..} =
  BeamDS.defaultDriverStats
    { BeamDS.driverId = getId driverId,
      BeamDS.idleSince = idleSince,
      BeamDS.totalRides = totalRides,
      BeamDS.totalDistance = totalDistance,
      BeamDS.ridesCancelled = ridesCancelled,
      BeamDS.totalRidesAssigned = totalRidesAssigned
    }
