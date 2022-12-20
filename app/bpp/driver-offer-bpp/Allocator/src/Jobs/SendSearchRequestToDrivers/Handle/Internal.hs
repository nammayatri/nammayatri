module Jobs.SendSearchRequestToDrivers.Handle.Internal
  ( isRideAlreadyAssigned,
    getRescheduleTime,
    receivedMinDriverQuotes,
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
import qualified Storage.Queries.DriverQuote as QDQ

isRideAlreadyAssigned :: Id SearchRequest -> Flow Bool
isRideAlreadyAssigned searchReqId = isJust <$> QB.findBySearchReq searchReqId

receivedMinDriverQuotes :: Id SearchRequest -> Flow Bool
receivedMinDriverQuotes searchReqId = do
  totalQuotesRecieved <- length <$> QDQ.findAllByRequestId searchReqId
  minDriverQuotesRequired <- asks (.driverPoolCfg.minDriverQuotesRequired)
  pure (totalQuotesRecieved >= minDriverQuotesRequired)

getRescheduleTime :: Flow UTCTime
getRescheduleTime = do
  now <- getCurrentTime
  singleBatchProcessTime <- fromIntegral <$> asks (.singleBatchProcessTime)
  return $ singleBatchProcessTime `addUTCTime` now
