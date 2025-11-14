module Storage.Queries.FleetOperatorDailyStatsExtra where

import Data.Time.Calendar (Day)
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import qualified Domain.Types.FleetOperatorDailyStats as DFODS
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.FleetOperatorDailyStats as Beam
import Storage.Queries.FleetOperatorDailyStats ()

-- Find records by fleetOperatorId, merchantLocalDate, and fleetDriverId IN [fleetOperatorId, driverId]
findByFleetOperatorIdAndDateWithDriverIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Day ->
  m [DFODS.FleetOperatorDailyStats]
findByFleetOperatorIdAndDateWithDriverIds fleetOperatorId driverId merchantLocalDate = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate,
          Se.Is Beam.fleetDriverId $ Se.In [fleetOperatorId, driverId]
        ]
    ]

-- Aggregate earnings by driverId for a fleet owner
data DriverEarningsAggregated = DriverEarningsAggregated
  { driverId :: Text,
    onlineTotalEarningSum :: Maybe HighPrecMoney,
    cashTotalEarningSum :: Maybe HighPrecMoney,
    cashPlatformFeesSum :: Maybe HighPrecMoney,
    onlinePlatformFeesSum :: Maybe HighPrecMoney,
    onlineDurationSum :: Maybe Seconds
  }
  deriving (Show, Generic)

mkDriverEarningsAggregated :: (Text, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Seconds) -> DriverEarningsAggregated
mkDriverEarningsAggregated (driverId, te, ct, cpf, opf, od) =
  DriverEarningsAggregated
    { driverId = driverId,
      onlineTotalEarningSum = te,
      cashTotalEarningSum = ct,
      cashPlatformFeesSum = cpf,
      onlinePlatformFeesSum = opf,
      onlineDurationSum = od
    }

sumDriverEarningsByFleetOwnerIdAndDriverIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  m [DriverEarningsAggregated]
sumDriverEarningsByFleetOwnerIdAndDriverIds fleetOwnerId driverIds fromDay toDay = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \stats ->
                ( B.group_ (Beam.fleetDriverId stats), -- Need to do group by with fleetOperatorId and fleetDriverId to get the correct stats
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.cashPlatformFees stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.onlinePlatformFees stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe Seconds) $ B.sum_ (B.coalesce_ [Beam.onlineDuration stats] (B.val_ (Seconds 0)))
                )
            )
            $ B.filter_'
              ( \stats ->
                  B.sqlBool_ (Beam.fleetOperatorId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.fleetDriverId stats `B.in_` (B.val_ <$> driverIds))
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.>=. B.val_ fromDay)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.<=. B.val_ toDay)
              )
              $ B.all_ (BeamCommon.fleetOperatorDailyStats BeamCommon.atlasDB)
  case res of
    Right results -> pure $ map (\(driverId, te, ct, cpf, opf, od) -> mkDriverEarningsAggregated (driverId, te, ct, cpf, opf, od)) results
    Left _ -> pure []

-- Aggregate metrics by driverId for a fleet owner
data DriverMetricsAggregated = DriverMetricsAggregated
  { driverId :: Text,
    onlineTotalEarningSum :: Maybe HighPrecMoney,
    cashTotalEarningSum :: Maybe HighPrecMoney,
    totalCompletedRidesSum :: Maybe Int,
    totalDistanceSum :: Maybe Meters,
    totalRequestCountSum :: Maybe Int,
    rejectedRequestCountSum :: Maybe Int,
    pulledRequestCountSum :: Maybe Int,
    acceptationRequestCountSum :: Maybe Int,
    driverCancellationCountSum :: Maybe Int,
    customerCancellationCountSum :: Maybe Int,
    onlineDurationSum :: Maybe Seconds,
    totalRatingScoreSum :: Maybe Int,
    rideDurationSum :: Maybe Seconds
  }
  deriving (Show, Generic)

mkDriverMetricsAggregated ::
  ( Text,
    Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe Int,
    Maybe Double,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Seconds,
    Maybe Int,
    Maybe Seconds
  ) ->
  DriverMetricsAggregated
mkDriverMetricsAggregated (driverId, te, ct, cr, td, tr, rr, pr, ar, dc, cc, od, trs, rd) =
  DriverMetricsAggregated
    { driverId = driverId,
      onlineTotalEarningSum = te,
      cashTotalEarningSum = ct,
      totalCompletedRidesSum = cr,
      totalDistanceSum = fmap (Meters . round) td,
      totalRequestCountSum = tr,
      rejectedRequestCountSum = rr,
      pulledRequestCountSum = pr,
      acceptationRequestCountSum = ar,
      driverCancellationCountSum = dc,
      customerCancellationCountSum = cc,
      onlineDurationSum = od,
      totalRatingScoreSum = trs,
      rideDurationSum = rd
    }

sumDriverMetricsByFleetOwnerIdAndDriverIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  m [DriverMetricsAggregated]
sumDriverMetricsByFleetOwnerIdAndDriverIds fleetOwnerId driverIds fromDay toDay = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \stats ->
                ( B.group_ (Beam.fleetDriverId stats), -- Need to do group by with fleetOperatorId and fleetDriverId to get the correct stats
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalCompletedRides stats] (B.val_ 0)),
                  B.as_ @(Maybe Double) $ B.sum_ (B.coalesce_ [Beam.totalDistance stats] (B.val_ 0.0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.rejectedRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.pulledRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.acceptationRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.driverCancellationCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.customerCancellationCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Seconds) $ B.sum_ (B.coalesce_ [Beam.onlineDuration stats] (B.val_ (Seconds 0))),
                  B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRatingScore stats] (B.val_ 0)),
                  B.as_ @(Maybe Seconds) $ B.sum_ (B.coalesce_ [Beam.rideDuration stats] (B.val_ (Seconds 0)))
                )
            )
            $ B.filter_'
              ( \stats ->
                  B.sqlBool_ (Beam.fleetOperatorId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.fleetDriverId stats `B.in_` (B.val_ <$> driverIds))
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.>=. B.val_ fromDay)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.<=. B.val_ toDay)
              )
              $ B.all_ (BeamCommon.fleetOperatorDailyStats BeamCommon.atlasDB)
  case res of
    Right results -> pure $ map (\(driverId, te, ct, cr, td, tr, rr, pr, ar, dc, cc, od, trs, rd) -> mkDriverMetricsAggregated (driverId, te, ct, cr, td, tr, rr, pr, ar, dc, cc, od, trs, rd)) results
    Left _ -> pure []
