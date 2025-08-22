{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DriverStats where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverStatsT f = DriverStatsT
  { driverId :: C f (Id DP.Person),
    totalRides :: C f Int,
    totalRatingScore :: C f (Maybe Int),
    validDriverCancellationTagCount :: C f Int
  }
  deriving (Generic)

deriving instance Show DriverStats

-- aggregated result for sums across multiple drivers

data DriverStatsAggregated = DriverStatsAggregated
  { totalRidesSum :: Int,
    totalRatingScoreSum :: Maybe Int,
    cancelledCount :: Int
  }
  deriving (Show, Generic)

mkDriverStatsAggregated :: (Int, Maybe Int, Int) -> DriverStatsAggregated
mkDriverStatsAggregated (tr, rs, cc) =
  DriverStatsAggregated
    { totalRidesSum = tr,
      totalRatingScoreSum = rs,
      cancelledCount = cc
    }

driverStatsTTable :: DriverStatsT (FieldModification DriverStatsT)
driverStatsTTable =
  DriverStatsT
    { driverId = "driver_id",
      totalRides = "total_rides",
      totalRatingScore = "total_rating_score",
      validDriverCancellationTagCount = "valid_driver_cancellation_tag_count"
    }

type DriverStats = DriverStatsT Identity

$(TH.mkClickhouseInstances ''DriverStatsT 'SELECT_FINAL_MODIFIER)

sumRatingAndTotalRidesByDriverIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  m DriverStatsAggregated
sumRatingAndTotalRidesByDriverIds driverIds = do
  res <-
    CH.findAll $
      CH.select_
        ( \ds ->
            CH.aggregate
              ( CH.sum_ ds.totalRides,
                CH.sum_ ds.totalRatingScore,
                CH.sum_ ds.validDriverCancellationTagCount
              )
        )
        $ CH.filter_
          ( \ds _ -> ds.driverId `CH.in_` driverIds
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverStatsTTable)
  pure $ maybe (DriverStatsAggregated 0 Nothing 0) mkDriverStatsAggregated (listToMaybe res)
