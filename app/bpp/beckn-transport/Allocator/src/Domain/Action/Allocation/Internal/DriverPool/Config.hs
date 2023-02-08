module Domain.Action.Allocation.Internal.DriverPool.Config
  ( DriverPoolBatchesConfig (..),
    HasDriverPoolBatchesConfig,
  )
where

import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

data DriverPoolBatchesConfig = DriverPoolBatchesConfig
  { driverBatchSize :: Int,
    maxNumberOfBatches :: Int
  }
  deriving (Generic, FromDhall)

type HasDriverPoolBatchesConfig r =
  ( HasField "driverPoolBatchesCfg" r DriverPoolBatchesConfig
  )
