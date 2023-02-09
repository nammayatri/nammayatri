module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config where

import EulerHS.Prelude hiding (id)
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (DriverPoolBatchesConfig)

data SendSearchRequestJobConfig = SendSearchRequestJobConfig
  { driverPoolBatchesCfg :: DriverPoolBatchesConfig,
    singleBatchProcessTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasSendSearchRequestJobConfig r =
  ( HasField "sendSearchRequestJobCfg" r SendSearchRequestJobConfig
  )
