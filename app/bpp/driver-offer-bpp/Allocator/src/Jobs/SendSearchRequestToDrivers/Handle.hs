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
  { isRideAlreadyAssigned :: m Bool,
    getNextDriverPoolBatch :: m [DriverPoolWithActualDistResult],
    prepareDriverPoolBatches :: m (),
    incrementPoolRadiusStep :: m (),
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> m (),
    getRescheduleTime :: m UTCTime
  }

handler :: HandleMonad m => Handle m -> m ExecutionResult
handler h@Handle {..} = do
  isRideAssigned <- isRideAlreadyAssigned
  if isRideAssigned
    then return Complete
    else processRequestSending h

processRequestSending :: HandleMonad m => Handle m -> m ExecutionResult
processRequestSending h@Handle {..} = do
  driverPool <- getDriverPoolBatch h
  sendSearchRequestToDrivers driverPool
  ReSchedule <$> getRescheduleTime

getDriverPoolBatch :: HandleMonad m => Handle m -> m [DriverPoolWithActualDistResult]
getDriverPoolBatch Handle {..} = do
  driverPoolBatch <- getNextDriverPoolBatch
  if not $ null driverPoolBatch
    then return driverPoolBatch
    else getNextRadiusBatch
  where
    getNextRadiusBatch = do
      incrementPoolRadiusStep
      prepareDriverPoolBatches
      getNextDriverPoolBatch
