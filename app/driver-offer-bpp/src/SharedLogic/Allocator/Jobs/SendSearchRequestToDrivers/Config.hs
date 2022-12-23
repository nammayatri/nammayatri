module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config where

import Beckn.Utils.Common
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude hiding (id)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config (DriverPoolBatchesConfig)

data SendSearchRequestJobConfig = SendSearchRequestJobConfig
  { driverPoolBatchesCfg :: DriverPoolBatchesConfig,
    singleBatchProcessTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasSendSearchRequestJobConfig r =
  ( HasField "sendSearchRequestJobCfg" r SendSearchRequestJobConfig
  )
