module Storage.Clickhouse.FleetOperatorStats where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common

-- Define the FleetOperatorStatsT data type

data FleetOperatorStatsT f = FleetOperatorStatsT
  { fleetOperatorId :: C f Text,
    totalRatingCount :: C f (Maybe Int),
    totalRatingScore :: C f (Maybe Int),
    driverFirstSubscription :: C f (Maybe Int),
    inspectionCompleted :: C f (Maybe Int),
    acceptationRequestCount :: C f (Maybe Int),
    totalRequestCount :: C f (Maybe Int),
    customerCancellationCount :: C f (Maybe Int),
    driverCancellationCount :: C f (Maybe Int),
    totalDistance :: C f (Maybe Double),
    totalCompletedRides :: C f (Maybe Int),
    totalEarning :: C f (Maybe HighPrecMoney)
  }
  deriving (Generic)

deriving instance Show FleetOperatorStats

fleetOperatorStatsTTable :: FleetOperatorStatsT (FieldModification FleetOperatorStatsT)
fleetOperatorStatsTTable =
  FleetOperatorStatsT
    { fleetOperatorId = "fleet_operator_id",
      totalRatingCount = "total_rating_count",
      totalRatingScore = "total_rating_score",
      driverFirstSubscription = "driver_first_subscription",
      inspectionCompleted = "inspection_completed",
      acceptationRequestCount = "acceptation_request_count",
      totalRequestCount = "total_request_count",
      customerCancellationCount = "customer_cancellation_count",
      driverCancellationCount = "driver_cancellation_count",
      totalDistance = "total_distance",
      totalCompletedRides = "total_completed_rides",
      totalEarning = "total_earning"
    }

type FleetOperatorStats = FleetOperatorStatsT Identity

$(TH.mkClickhouseInstances ''FleetOperatorStatsT 'SELECT_FINAL_MODIFIER)

data OperatorStatsAggregated = OperatorStatsAggregated
  { totalRatingScoreSum :: Maybe Int,
    totalRatingCountSum :: Maybe Int,
    acceptationRequestCountSum :: Maybe Int,
    totalRequestCountSum :: Maybe Int,
    driverCancellationCountSum :: Maybe Int,
    totalCompletedRidesSum :: Maybe Int
  }
  deriving (Show, Generic)

mkOperatorStatsAggregated :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> OperatorStatsAggregated
mkOperatorStatsAggregated (trs, trn, arc, trc, dcc, tcr) =
  OperatorStatsAggregated
    { totalRatingScoreSum = trs,
      totalRatingCountSum = trn,
      acceptationRequestCountSum = arc,
      totalRequestCountSum = trc,
      driverCancellationCountSum = dcc,
      totalCompletedRidesSum = tcr
    }

sumStatsByFleetOperatorId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  m OperatorStatsAggregated
sumStatsByFleetOperatorId fleetOperatorId = do
  res <-
    CH.findAll $
      CH.select_
        ( \fos ->
            CH.aggregate
              ( CH.sum_ fos.totalRatingScore,
                CH.sum_ fos.totalRatingCount,
                CH.sum_ fos.acceptationRequestCount,
                CH.sum_ fos.totalRequestCount,
                CH.sum_ fos.driverCancellationCount,
                CH.sum_ fos.totalCompletedRides
              )
        )
        $ CH.filter_
          ( \fos -> fos.fleetOperatorId CH.==. fleetOperatorId
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fleetOperatorStatsTTable)
  pure $ maybe (OperatorStatsAggregated Nothing Nothing Nothing Nothing Nothing Nothing) mkOperatorStatsAggregated (listToMaybe res)
