module SharedLogic.DriverMode where

import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverInformation as DI
import Kernel.Prelude

getDriverStatus :: Maybe DI.DriverMode -> Bool -> DDFS.FlowStatus
getDriverStatus mode isActive =
  case mode of
    Just DI.ONLINE -> DDFS.ACTIVE
    Just DI.SILENT -> DDFS.SILENT
    Just DI.OFFLINE -> DDFS.IDLE
    Nothing -> do
      if isActive
        then DDFS.ACTIVE
        else DDFS.IDLE
