module SharedLogic.DriverPool.Config where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)

data DriverPoolConfig = DriverPoolConfig
  { defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds
  }
  deriving (Generic, FromDhall)

type HasDriverPoolConfig r =
  ( HasField "driverPoolCfg" r DriverPoolConfig
  )
