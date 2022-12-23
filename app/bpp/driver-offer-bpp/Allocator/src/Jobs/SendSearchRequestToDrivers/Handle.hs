module Jobs.SendSearchRequestToDrivers.Handle
  ( HandleMonad,
    Handle (..),
    MetricsHandle (..),
    handler,
  )
where

import Beckn.Prelude
import Beckn.Utils.Common
import Lib.Scheduler.Types (ExecutionResult (..))
import SharedLogic.DriverPool

type HandleMonad m = (Monad m, MonadClock m, Log m)

data MetricsHandle m = MetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m ()
  }

data Handle m = Handle
  { isBatchNumExceedLimit :: m Bool,
    isRideAlreadyAssigned :: m Bool,
    isReceivedMaxDriverQuotes :: m Bool,
    getNextDriverPoolBatch :: m [DriverPoolWithActualDistResult],
    cleanupDriverPoolBatches :: m (),
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> m (),
    getRescheduleTime :: m UTCTime,
    metrics :: MetricsHandle m,
    setBatchDurationLock :: m (Maybe UTCTime),
    createRescheduleTime :: UTCTime -> m UTCTime
  }

handler :: HandleMonad m => Handle m -> m ExecutionResult
handler h@Handle {..} = do
  logInfo "Starting job execution"
  metrics.incrementTaskCounter
  measuringDuration (\ms _ -> metrics.putTaskDuration ms) $ do
    isRideAssigned <- isRideAlreadyAssigned
    res <-
      if isRideAssigned
        then do
          logInfo "Ride already assigned."
          return Complete
        else do
          isReceivedMaxDriverQuotes' <- isReceivedMaxDriverQuotes
          if isReceivedMaxDriverQuotes'
            then do
              logInfo "Received enough quotes from drivers."
              return Complete
            else processRequestSending h
    case res of
      Complete -> cleanupDriverPoolBatches
      _ -> return ()
    return res

processRequestSending :: HandleMonad m => Handle m -> m ExecutionResult
processRequestSending Handle {..} = do
  mLastProcTime <- setBatchDurationLock
  case mLastProcTime of
    Just lastProcTime -> ReSchedule <$> createRescheduleTime lastProcTime
    Nothing -> do
      isBatchNumExceedLimit' <- isBatchNumExceedLimit
      if isBatchNumExceedLimit'
        then do
          metrics.incrementFailedTaskCounter
          logInfo "No driver accepted"
          return Complete
        else do
          driverPool <- getNextDriverPoolBatch
          if null driverPool
            then do
              metrics.incrementFailedTaskCounter
              logInfo "No driver available"
              return Complete
            else do
              sendSearchRequestToDrivers driverPool
              ReSchedule <$> getRescheduleTime