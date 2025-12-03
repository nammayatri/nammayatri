module Storage.Queries.FleetOperatorDailyStatsExtra where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import Data.Time.Calendar (Day)
import qualified Database.Beam as B
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
                ( B.group_ (Beam.fleetOperatorId stats),
                  B.group_ (Beam.fleetDriverId stats),
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
    Right results -> pure $ map (\(_, fleetDriverId, te, ct, cpf, opf, od) -> mkDriverEarningsAggregated (fleetDriverId, te, ct, cpf, opf, od)) results
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure. Error: "
            <> show err
        )
      pure []

-- Aggregate metrics by driverId for a fleet owner
data DailyFleetMetricsAggregated = DailyFleetMetricsAggregated
  { totalEarningSum :: Maybe HighPrecMoney,
    totalCompletedRidesSum :: Maybe Int,
    totalDistanceSum :: Maybe Meters,
    totalRequestCountSum :: Maybe Int,
    rejectedRequestCountSum :: Maybe Int,
    pulledRequestCountSum :: Maybe Int,
    acceptationRequestCountSum :: Maybe Int,
    driverCancellationCountSum :: Maybe Int,
    customerCancellationCountSum :: Maybe Int
  }
  deriving (Show, Generic)

mkDailyFleetMetricsAggregated ::
  ( Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe Int,
    Maybe Meters,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int,
    Maybe Int
  ) ->
  DailyFleetMetricsAggregated
mkDailyFleetMetricsAggregated (ote, cte, cr, td, tr, rr, pr, ar, dc, cc) =
  DailyFleetMetricsAggregated
    { totalEarningSum = getTotalEarningSum ote cte,
      totalCompletedRidesSum = cr,
      totalDistanceSum = td,
      totalRequestCountSum = tr,
      rejectedRequestCountSum = rr,
      pulledRequestCountSum = pr,
      acceptationRequestCountSum = ar,
      driverCancellationCountSum = dc,
      customerCancellationCountSum = cc
    }

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
  ( Text, -- fleetOperatorId
    Text,
    (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe Int, Maybe Double),
    (Maybe Int, Maybe Int, Maybe Int, Maybe Int),
    (Maybe Int, Maybe Int, Maybe Seconds, Maybe Int),
    Maybe Seconds
  ) ->
  DriverMetricsAggregated
mkDriverMetricsAggregated (_, driverId, (te, ct, cr, td), (tr, rr, pr, ar), (dc, cc, od, trs), rd) =
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

sumDriverMetricsByFleetOwnerIdAndDriverIds :: -- Should we use clickhouse?
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
                ( B.group_ (Beam.fleetOperatorId stats),
                  B.group_ (Beam.fleetDriverId stats),
                  ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                    B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalCompletedRides stats] (B.val_ 0)),
                    B.as_ @(Maybe Double) $ B.sum_ (B.coalesce_ [Beam.totalDistance stats] (B.val_ 0.0))
                  ),
                  ( B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRequestCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.rejectedRequestCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.pulledRequestCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.acceptationRequestCount stats] (B.val_ 0))
                  ),
                  ( B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.driverCancellationCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.customerCancellationCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Seconds) $ B.sum_ (B.coalesce_ [Beam.onlineDuration stats] (B.val_ (Seconds 0))),
                    B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRatingScore stats] (B.val_ 0))
                  ),
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
    Right results -> pure $ map mkDriverMetricsAggregated results
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure. Error: "
            <> show err
        )
      pure []

sumDriverEarningsByFleetOwnerIdAndDriverIdsDB ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.FleetDriverListStatsSortOn ->
  m [DriverEarningsAggregated]
sumDriverEarningsByFleetOwnerIdAndDriverIdsDB _ [] _ _ _ _ _ _ = pure []
sumDriverEarningsByFleetOwnerIdAndDriverIdsDB fleetOwnerId driverIds fromDay toDay limit offset mbSortDesc mbSortOn = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (orderClause sortDescFlag mbSortOn) $
                B.aggregate_
                  ( \stats ->
                      ( B.group_ (Beam.fleetOperatorId stats),
                        B.group_ (Beam.fleetDriverId stats),
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
    Right results -> pure $ map (\(_, fleetDriverId, te, ct, cpf, opf, od) -> mkDriverEarningsAggregated (fleetDriverId, te, ct, cpf, opf, od)) results
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure. Error: "
            <> show err
        )
      pure []
  where
    sortDescFlag = fromMaybe False mbSortDesc

    orderClause sortFlag sortOnField (_, _, onlineEarning, cashEarning, cashFees, onlineFees, _onlineDuration) =
      let column =
            case sortOnField of
              Just Common.CASH_TOTAL_EARNING -> cashEarning
              Just Common.CASH_PLATFORM_FEES -> cashFees
              Just Common.ONLINE_PLATFORM_FEES -> onlineFees
              -- Just Common.ONLINE_DURATION -> onlineDuration
              _ -> onlineEarning
       in if sortFlag then B.desc_ column else B.asc_ column

sumDriverMetricsByFleetOwnerIdAndDriverIdsDB ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  [Text] ->
  Day ->
  Day ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Common.FleetDriverListStatsSortOn ->
  m [DriverMetricsAggregated]
sumDriverMetricsByFleetOwnerIdAndDriverIdsDB _ [] _ _ _ _ _ _ = pure []
sumDriverMetricsByFleetOwnerIdAndDriverIdsDB fleetOwnerId driverIds fromDay toDay limit offset mbSortDesc mbSortOn = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (orderClauseMetrics sortDescFlag mbSortOn) $
                B.aggregate_
                  ( \stats ->
                      ( B.group_ (Beam.fleetOperatorId stats),
                        B.group_ (Beam.fleetDriverId stats),
                        ( B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                          B.as_ @(Maybe HighPrecMoney) $ B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalCompletedRides stats] (B.val_ 0)),
                          B.as_ @(Maybe Double) $ B.sum_ (B.coalesce_ [Beam.totalDistance stats] (B.val_ 0.0))
                        ),
                        ( B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRequestCount stats] (B.val_ 0)),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.rejectedRequestCount stats] (B.val_ 0)),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.pulledRequestCount stats] (B.val_ 0)),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.acceptationRequestCount stats] (B.val_ 0))
                        ),
                        ( B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.driverCancellationCount stats] (B.val_ 0)),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.customerCancellationCount stats] (B.val_ 0)),
                          B.as_ @(Maybe Seconds) $ B.sum_ (B.coalesce_ [Beam.onlineDuration stats] (B.val_ (Seconds 0))),
                          B.as_ @(Maybe Int) $ B.sum_ (B.coalesce_ [Beam.totalRatingScore stats] (B.val_ 0))
                        ),
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
    Right results -> pure $ map mkDriverMetricsAggregated results
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure. Error: "
            <> show err
        )
      pure []
  where
    sortDescFlag = fromMaybe False mbSortDesc

    orderClauseMetrics sortFlag sortOnField (_, _, (_, _, completed, _), (totalReq, rejected, pulled, accepted), (driverCancelled, customerCancelled, _onlineDuration, totalRatingScore), _rideDuration) =
      let column =
            case sortOnField of
              Just Common.TOTAL_COMPLETED_RIDES -> completed
              Just Common.TOTAL_REQUEST_COUNT -> totalReq
              Just Common.REJECTED_REQUEST_COUNT -> rejected
              Just Common.PULLED_REQUEST_COUNT -> pulled
              Just Common.ACCEPTATION_REQUEST_COUNT -> accepted
              Just Common.DRIVER_CANCELLATION_COUNT -> driverCancelled
              Just Common.CUSTOMER_CANCELLATION_COUNT -> customerCancelled
              -- Just Common.ONLINE_DURATION -> onlineDuration
              Just Common.TOTAL_RATING_SCORE -> totalRatingScore
              -- Just Common.RIDE_DURATION -> rideDuration
              _ -> completed
       in if sortFlag then B.desc_ column else B.asc_ column

sumFleetMetricsByFleetOwnerIdAndDateRangeDB ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Day ->
  Day ->
  m DailyFleetMetricsAggregated
sumFleetMetricsByFleetOwnerIdAndDateRangeDB fleetOwnerId fromDay toDay = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \stats ->
                ( B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe Int) $
                    B.sum_ (B.coalesce_ [Beam.totalCompletedRides stats] (B.val_ 0)),
                  B.as_ @(Maybe Double) $
                    B.sum_ (B.coalesce_ [Beam.totalDistance stats] (B.val_ (0.0))),
                  B.as_ @(Maybe Int) $
                    B.sum_ (B.coalesce_ [Beam.totalRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $
                    B.sum_ (B.coalesce_ [Beam.rejectedRequestCount stats] (B.val_ 0)),
                  B.as_ @(Maybe Int) $
                    B.sum_ (B.coalesce_ [Beam.pulledRequestCount stats] (B.val_ 0)),
                  ( B.as_ @(Maybe Int) $
                      B.sum_ (B.coalesce_ [Beam.acceptationRequestCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $
                      B.sum_ (B.coalesce_ [Beam.driverCancellationCount stats] (B.val_ 0)),
                    B.as_ @(Maybe Int) $
                      B.sum_ (B.coalesce_ [Beam.customerCancellationCount stats] (B.val_ 0))
                  )
                )
            )
            $ B.filter_'
              ( \stats ->
                  B.sqlBool_ (Beam.fleetOperatorId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.fleetDriverId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.>=. B.val_ fromDay)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.<=. B.val_ toDay)
              )
              $ B.all_ (BeamCommon.fleetOperatorDailyStats BeamCommon.atlasDB)
  case res of
    Right rows ->
      case listToMaybe rows of
        Just (ote, cte, cr, td, tr, rr, pr, (ar, dc, cc)) ->
          pure $ mkDailyFleetMetricsAggregated (ote, cte, cr, fmap (Meters . round) td, tr, rr, pr, ar, dc, cc)
        Nothing ->
          pure $
            DailyFleetMetricsAggregated
              { totalEarningSum = Nothing,
                totalCompletedRidesSum = Nothing,
                totalDistanceSum = Nothing,
                totalRequestCountSum = Nothing,
                rejectedRequestCountSum = Nothing,
                pulledRequestCountSum = Nothing,
                acceptationRequestCountSum = Nothing,
                driverCancellationCountSum = Nothing,
                customerCancellationCountSum = Nothing
              }
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure in sumFleetMetricsByFleetOwnerIdAndDateRangeDB. Error: "
            <> show err
        )
      pure $
        DailyFleetMetricsAggregated
          { totalEarningSum = Nothing,
            totalCompletedRidesSum = Nothing,
            totalDistanceSum = Nothing,
            totalRequestCountSum = Nothing,
            rejectedRequestCountSum = Nothing,
            pulledRequestCountSum = Nothing,
            acceptationRequestCountSum = Nothing,
            driverCancellationCountSum = Nothing,
            customerCancellationCountSum = Nothing
          }

-- | Aggregated earnings for a fleet owner over a date range (DB based).
data DailyFleetEarningsAggregated = DailyFleetEarningsAggregated
  { totalEarningSum :: Maybe HighPrecMoney,
    cashPlatformFeesSum :: Maybe HighPrecMoney,
    onlinePlatformFeesSum :: Maybe HighPrecMoney,
    onlineDurationSum :: Maybe Seconds
  }
  deriving (Show, Generic)

getTotalEarningSum :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney
getTotalEarningSum mote mcte = case (mote, mcte) of
  (Just ote, Just cte) -> Just (ote + cte)
  (Just ote, Nothing) -> Just ote
  (Nothing, Just cte) -> Just cte
  (Nothing, Nothing) -> Nothing

mkDailyFleetEarningsAggregated ::
  ( Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe HighPrecMoney,
    Maybe Seconds
  ) ->
  DailyFleetEarningsAggregated
mkDailyFleetEarningsAggregated (ote, cte, cpf, opf, od) =
  DailyFleetEarningsAggregated
    { totalEarningSum = getTotalEarningSum ote cte,
      cashPlatformFeesSum = cpf,
      onlinePlatformFeesSum = opf,
      onlineDurationSum = od
    }

sumFleetEarningsByFleetOwnerIdAndDateRangeDB ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Day ->
  Day ->
  m DailyFleetEarningsAggregated
sumFleetEarningsByFleetOwnerIdAndDateRangeDB fleetOwnerId fromDay toDay = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_
            ( \stats ->
                ( B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.onlineTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.cashTotalEarning stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.cashPlatformFees stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe HighPrecMoney) $
                    B.sum_ (B.coalesce_ [Beam.onlinePlatformFees stats] (B.val_ (HighPrecMoney 0.0))),
                  B.as_ @(Maybe Seconds) $
                    B.sum_ (B.coalesce_ [Beam.onlineDuration stats] (B.val_ (Seconds 0)))
                )
            )
            $ B.filter_'
              ( \stats ->
                  B.sqlBool_ (Beam.fleetOperatorId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.fleetDriverId stats B.==. B.val_ fleetOwnerId)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.>=. B.val_ fromDay)
                    B.&&?. B.sqlBool_ (Beam.merchantLocalDate stats B.<=. B.val_ toDay)
              )
              $ B.all_ (BeamCommon.fleetOperatorDailyStats BeamCommon.atlasDB)
  case res of
    Right rows ->
      case listToMaybe rows of
        Just (onlineTot, cashTot, cashFees, onlineFees, od) ->
          pure $ mkDailyFleetEarningsAggregated (onlineTot, cashTot, cashFees, onlineFees, od)
        Nothing ->
          pure $ DailyFleetEarningsAggregated Nothing Nothing Nothing Nothing
    Left err -> do
      logTagError
        "FleetOperatorDailyStats"
        ( "DB failure in sumFleetEarningsByFleetOwnerIdAndDateRangeDB. Error: "
            <> show err
        )
      pure $ DailyFleetEarningsAggregated Nothing Nothing Nothing Nothing
