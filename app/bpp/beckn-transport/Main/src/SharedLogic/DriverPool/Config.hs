module SharedLogic.DriverPool.Config where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)

data DriverPoolConfig = DriverPoolConfig
  { defaultRadiusOfSearch :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds
  }
  deriving (Generic, FromDhall)

type HasDriverPoolConfig r =
  ( HasField "driverPoolCfg" r DriverPoolConfig
  )
