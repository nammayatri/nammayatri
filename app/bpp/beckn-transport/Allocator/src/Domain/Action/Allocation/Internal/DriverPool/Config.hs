module Domain.Action.Allocation.Internal.DriverPool.Config
  ( DriverPoolBatchesConfig (..),
    HasDriverPoolBatchesConfig,
  )
where

import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude hiding (id)

data DriverPoolBatchesConfig = DriverPoolBatchesConfig
  { driverBatchSize :: Int,
    maxNumberOfBatches :: Int
  }
  deriving (Generic, FromDhall)

type HasDriverPoolBatchesConfig r =
  ( HasField "driverPoolBatchesCfg" r DriverPoolBatchesConfig
  )
