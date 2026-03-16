{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FleetRcDailyStatsExtra where

import Data.Time.Calendar (Day)
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Logging
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetRcDailyStats as Beam
import Storage.Queries.OrphanInstances.FleetRcDailyStats ()

data FleetRcDailyStatsAggregated = FleetRcDailyStatsAggregated
  { fleetOwnerId' :: Text,
    rcId :: Text,
    totalEarnings :: HighPrecMoney,
    totalCompletedRides :: Int,
    totalDistance :: Meters,
    totalDuration :: Seconds
  }
  deriving (Show, Generic)

mkFleetRcDailyStatsAggregated :: Text -> Text -> HighPrecMoney -> Int -> Meters -> Seconds -> FleetRcDailyStatsAggregated
mkFleetRcDailyStatsAggregated fleetOwnerId' rcId totalEarnings totalCompletedRides totalDistance totalDuration =
  FleetRcDailyStatsAggregated {..}

sumVehicleStatsByFleetOwnerIdAndDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Text ->
  Maybe Text ->
  Int ->
  Int ->
  Day ->
  Day ->
  m [FleetRcDailyStatsAggregated]
sumVehicleStatsByFleetOwnerIdAndDateRange fleetOwnerId mbRcId limit offset fromDay toDay = do
  dbConf <- getReplicaBeamConfig

  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.aggregate_
                ( \stats ->
                    ( B.group_ (Beam.fleetOwnerId stats),
                      B.group_ (Beam.rcId stats),
                      B.coalesce_ [B.sum_ (Beam.totalEarnings stats)] (B.val_ (HighPrecMoney 0)),
                      B.coalesce_ [B.sum_ (Beam.totalCompletedRides stats)] (B.val_ 0),
                      B.coalesce_ [B.sum_ (Beam.rideDistance stats)] (B.val_ 0.0),
                      B.coalesce_ [B.sum_ (Beam.rideDuration stats)] (B.val_ (Seconds 0))
                    )
                )
                $ B.filter_'
                  ( \stats ->
                      B.sqlBool_ (Beam.fleetOwnerId stats B.==. B.val_ fleetOwnerId)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\rcId -> B.sqlBool_ (Beam.rcId stats B.==. B.val_ rcId)) mbRcId
                        B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.>=. B.val_ fromDay)
                        B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.<=. B.val_ toDay)
                  )
                  $ B.all_ (BeamCommon.fleetRcDailyStats BeamCommon.atlasDB)

  case res of
    Right result -> pure $ map (\(fleetOwnerId', rcId, totalEarnings, totalCompletedRides, totalDistance, totalDuration) -> mkFleetRcDailyStatsAggregated fleetOwnerId' rcId totalEarnings totalCompletedRides (Meters $ round totalDistance) totalDuration) result
    Left err -> do
      logTagError "FleetOperatorDailyStats" ("DB failure. Error: " <> show err)
      pure []
