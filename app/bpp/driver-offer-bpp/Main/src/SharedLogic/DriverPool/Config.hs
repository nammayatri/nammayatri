module SharedLogic.DriverPool.Config where

import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Dhall (FromDhall)

data IntelligentPoolConfig = IntelligentPoolConfig
  { minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions
  }
  deriving (Generic, FromDhall)

data RideRequestPopupConfig = RideRequestPopupConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    defaultPopupDelay :: Seconds,
    thresholdCancellationScore :: Maybe Int,
    thresholdRidesCount :: Maybe Int
  }
  deriving (Generic, FromDhall)

data DriverPoolConfig = DriverPoolConfig
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    intelligentPoolPercentage :: Maybe Int
  }
  deriving (Generic, FromDhall)

data OverrideDriverPoolConfigRange = OverrideDriverPoolConfigRange
  { startDistance :: Meters,
    endDistance :: Maybe Meters
  }
  deriving (Generic, FromDhall)

data OverrideDriverPoolConfig = OverrideDriverPoolConfig
  { configRange :: OverrideDriverPoolConfigRange,
    driverPoolCfg :: DriverPoolConfig
  }
  deriving (Generic, FromDhall)

type HasDriverPoolConfig r =
  ( HasField "driverPoolCfg" r DriverPoolConfig,
    HasField "overrideDriverPoolConfig" r [OverrideDriverPoolConfig],
    HasField "intelligentPoolConfig" r IntelligentPoolConfig,
    HasField "rideRequestPopupConfig" r RideRequestPopupConfig,
    HasMaxParallelSearchRequests r
  )

type HasMaxParallelSearchRequests r =
  ( HasField "maxParallelSearchRequests" r Int
  )

getDriverPoolConfig :: (MonadFlow m, MonadReader r m, HasDriverPoolConfig r) => Meters -> m DriverPoolConfig
getDriverPoolConfig dist = do
  defaultConfig <- asks (.driverPoolCfg)
  overrideConfigs <- asks (.overrideDriverPoolConfig)
  let applicableConfig = find filterByDist overrideConfigs
  return $ maybe defaultConfig (.driverPoolCfg) applicableConfig
  where
    filterByDist cfg =
      (dist >= cfg.configRange.startDistance)
        && maybe True (dist <=) cfg.configRange.endDistance
