module SharedLogic.DriverPool.Config where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)

data DriverPoolConfig = DriverPoolConfig
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int
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
    HasField "overrideDriverPoolConfig" r [OverrideDriverPoolConfig]
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
