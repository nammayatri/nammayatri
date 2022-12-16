module Jobs.SendSearchRequestToDrivers.Handle
  ( HandleMonad,
    Handle (..),
    handler,
  )
where

import Beckn.Prelude
import Lib.Scheduler.Types (ExecutionResult (..))
import SharedLogic.DriverPool

type HandleMonad m = (Monad m)

data Handle m = Handle
  { isBatchNumExceedLimit :: m Bool,
    isRideAlreadyAssigned :: m Bool,
    getNextDriverPoolBatch :: m [DriverPoolWithActualDistResult],
    cleanupDriverPoolBatches :: m (),
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> m (),
    getRescheduleTime :: m UTCTime
  }

handler :: HandleMonad m => Handle m -> m ExecutionResult
handler h@Handle {..} = do
  isRideAssigned <- isRideAlreadyAssigned
  res <-
    if isRideAssigned
      then return Complete
      else processRequestSending h
  case res of
    Complete -> cleanupDriverPoolBatches
    _ -> return ()
  return res

processRequestSending :: HandleMonad m => Handle m -> m ExecutionResult
processRequestSending Handle {..} = do
  isBatchNumExceedLimit' <- isBatchNumExceedLimit
  if isBatchNumExceedLimit'
    then return Complete -- No driver accepted
    else do
      driverPool <- getNextDriverPoolBatch
      if null driverPool
        then return Complete -- No driver available
        else do
          sendSearchRequestToDrivers driverPool
          ReSchedule <$> getRescheduleTime
