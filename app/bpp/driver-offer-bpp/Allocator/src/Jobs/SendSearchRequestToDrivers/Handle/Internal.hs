module Jobs.SendSearchRequestToDrivers.Handle.Internal
  ( isRideAlreadyAssigned,
    getRescheduleTime,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.SearchRequest
import Environment (Flow)
import Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool as Reexport
import Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers as Reexport
import qualified Storage.Queries.Booking as QB

isRideAlreadyAssigned :: Id SearchRequest -> Flow Bool
isRideAlreadyAssigned searchReqId = isJust <$> QB.findBySearchReq searchReqId

getRescheduleTime :: Flow UTCTime
getRescheduleTime = do
  now <- getCurrentTime
  singleBatchProcessTime <- fromIntegral <$> asks (.singleBatchProcessTime)
  return $ singleBatchProcessTime `addUTCTime` now
