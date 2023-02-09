module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
  ( DriverPoolBatchesConfig (..),
    HasDriverPoolBatchesConfig,
    PoolSortingType (..),
  )
where

import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

data DriverPoolBatchesConfig = DriverPoolBatchesConfig
  { driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    minDriverBatchSize :: Int,
    poolSortingType :: PoolSortingType
  }
  deriving (Generic, FromDhall)

type HasDriverPoolBatchesConfig r =
  ( HasField "driverPoolBatchesCfg" r DriverPoolBatchesConfig
  )

data PoolSortingType = Intelligent | Random
  deriving (Generic, FromDhall)
