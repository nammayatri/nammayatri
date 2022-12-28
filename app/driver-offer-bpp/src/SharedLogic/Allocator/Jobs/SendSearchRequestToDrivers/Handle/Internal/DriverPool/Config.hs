module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
  ( DriverPoolBatchesConfig (..),
    HasDriverPoolBatchesConfig,
    PoolSortingType (..),
  )
where

import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude hiding (id)

data DriverPoolBatchesConfig = DriverPoolBatchesConfig
  { driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    poolSortingType :: PoolSortingType
  }
  deriving (Generic, FromDhall)

type HasDriverPoolBatchesConfig r =
  ( HasField "driverPoolBatchesCfg" r DriverPoolBatchesConfig
  )

data PoolSortingType = Intelligent | Random
  deriving (Generic, FromDhall)
