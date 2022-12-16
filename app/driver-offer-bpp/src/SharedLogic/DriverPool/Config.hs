module SharedLogic.DriverPool.Config where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import SharedLogic.DriverPool.Types (PoolRadiusStep)

data DriverPoolConfig = DriverPoolConfig
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStep :: PoolRadiusStep,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters
  }
  deriving (Generic, FromDhall)

type HasDriverPoolConfig r =
  ( HasField "driverPoolCfg" r DriverPoolConfig
  )
