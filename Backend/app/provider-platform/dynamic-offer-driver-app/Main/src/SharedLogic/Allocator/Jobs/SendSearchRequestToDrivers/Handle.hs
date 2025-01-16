{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
  ( HandleMonad,
    Handle (..),
    MetricsHandle (..),
    handler,
  )
where

import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Scheduler.Types (ExecutionResult (..))
import SharedLogic.DriverPool

type HandleMonad m = (MonadClock m, Log m)

data MetricsHandle m = MetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m ()
  }

data Handle m = Handle
  { isBatchNumExceedLimit :: m Bool,
    isReceivedMaxDriverQuotes :: m Bool,
    getNextDriverPoolBatch :: GoHomeConfig -> m DriverPoolWithActualDistResultWithFlags,
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> [Id Driver] -> GoHomeConfig -> m (),
    getRescheduleTime :: m UTCTime,
    metrics :: MetricsHandle m,
    isSearchTryValid :: m Bool,
    isBookingValid :: Bool,
    initiateDriverSearchBatch :: m (),
    cancelSearchTry :: m (),
    cancelBookingIfApplies :: m (),
    isScheduledBooking :: Bool
  }

handler :: HandleMonad m => Handle m -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds)
handler h@Handle {..} goHomeCfg = do
  logInfo "Starting job execution"
  metrics.incrementTaskCounter
  measuringDuration (\ms (_, _, _) -> metrics.putTaskDuration ms) $ do
    isSearchTryValid' <- isSearchTryValid
    if not isSearchTryValid' || not isBookingValid
      then do
        logInfo "Search request is either assigned, cancelled or expired."
        return (Complete, NormalPool, Nothing)
      else do
        isReceivedMaxDriverQuotes' <- isReceivedMaxDriverQuotes
        if isReceivedMaxDriverQuotes'
          then do
            logInfo "Received enough quotes from drivers."
            return (Complete, NormalPool, Nothing)
          else processRequestSending h goHomeCfg

processRequestSending :: HandleMonad m => Handle m -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds)
processRequestSending Handle {..} goHomeCfg = do
  isBatchNumExceedLimit' <- isBatchNumExceedLimit
  if isBatchNumExceedLimit'
    then do
      if isScheduledBooking
        then do
          initiateDriverSearchBatch
          return (Complete, NormalPool, Nothing)
        else do
          metrics.incrementFailedTaskCounter
          logInfo "No driver accepted"
          cancelSearchTry
          cancelBookingIfApplies
          return (Complete, NormalPool, Nothing)
    else do
      driverPoolWithFlags <- getNextDriverPoolBatch goHomeCfg
      when (not $ null driverPoolWithFlags.driverPoolWithActualDistResult) $
        sendSearchRequestToDrivers driverPoolWithFlags.driverPoolWithActualDistResult driverPoolWithFlags.prevBatchDrivers goHomeCfg
      ReSchedule <$> getRescheduleTime <&> (,driverPoolWithFlags.poolType,driverPoolWithFlags.nextScheduleTime)
