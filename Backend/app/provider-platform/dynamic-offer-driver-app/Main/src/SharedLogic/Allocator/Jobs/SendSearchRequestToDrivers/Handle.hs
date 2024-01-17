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
import Domain.Types.SearchRequest
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
    getPoolBatchNum :: m Int,
    isReceivedMaxDriverQuotes :: m Bool,
    getNextDriverPoolBatch :: GoHomeConfig -> m DriverPoolWithActualDistResultWithFlags,
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> [Id Driver] -> GoHomeConfig -> m (),
    getRescheduleTime :: m UTCTime,
    getRescheduleTimeRental :: NominalDiffTime -> UTCTime -> m UTCTime,
    metrics :: MetricsHandle m,
    setBatchDurationLock :: m (Maybe UTCTime),
    createRescheduleTime :: UTCTime -> m UTCTime,
    isSearchTryValid :: m Bool,
    cancelSearchTry :: m ()
  }

handler :: (HandleMonad m, MonadTime m) => Handle m -> GoHomeConfig -> m (ExecutionResult, Bool)
handler h@Handle {..} goHomeCfg = do
  logInfo "Starting job execution"
  metrics.incrementTaskCounter
  measuringDuration (\ms (_, _) -> metrics.putTaskDuration ms) $ do
    isSearchTryValid' <- isSearchTryValid
    if not isSearchTryValid'
      then do
        logInfo "Search request is either assigned, cancelled or expired."
        return (Complete, False)
      else do
        isReceivedMaxDriverQuotes' <- isReceivedMaxDriverQuotes
        if isReceivedMaxDriverQuotes'
          then do
            logInfo "Received enough quotes from drivers."
            return (Complete, False)
          else processRequestSending h goHomeCfg

processRequestSending :: (HandleMonad m, MonadTime m) => Handle m -> GoHomeConfig -> m (ExecutionResult, Bool)
processRequestSending Handle {..} goHomeCfg = do
  mLastProcTime <- setBatchDurationLock
  currentBatchNum <- getPoolBatchNum
  driverPoolWithFlags <- getNextDriverPoolBatch goHomeCfg
  case mLastProcTime of
    Just lastProcTime -> case driverPoolWithFlags.searchRequestTag of
      ON_DEMAND -> ReSchedule <$> createRescheduleTime lastProcTime <&> (,False)
      RENTAL -> do
        case driverPoolWithFlags.allocateRentalRideTimeDiff !? currentBatchNum of
          Just timeDiff -> ReSchedule <$> getRescheduleTimeRental (intToNominalDiffTime timeDiff) lastProcTime <&> (,False)
          Nothing -> do
            logInfo "No rental driver available"
            cancelSearchTry
            return (Complete, False)
    Nothing -> do
      isBatchNumExceedLimit' <- isBatchNumExceedLimit
      if isBatchNumExceedLimit'
        then do
          metrics.incrementFailedTaskCounter
          logInfo "No driver accepted"
          cancelSearchTry
          return (Complete, False)
        else do
          if null driverPoolWithFlags.driverPoolWithActualDistResult
            then do
              metrics.incrementFailedTaskCounter
              logInfo "No driver available"
              cancelSearchTry
              return (Complete, driverPoolWithFlags.isGoHomeBatch)
            else do
              sendSearchRequestToDrivers driverPoolWithFlags.driverPoolWithActualDistResult driverPoolWithFlags.prevBatchDrivers goHomeCfg
              now <- getCurrentTime
              case driverPoolWithFlags.searchRequestTag of
                ON_DEMAND -> ReSchedule <$> getRescheduleTime <&> (,driverPoolWithFlags.isGoHomeBatch)
                RENTAL -> do
                  case driverPoolWithFlags.allocateRentalRideTimeDiff !? currentBatchNum of
                    Just timeDiff -> ReSchedule <$> getRescheduleTimeRental (intToNominalDiffTime timeDiff) now <&> (,False)
                    Nothing -> do
                      logInfo "No rental driver available"
                      cancelSearchTry
                      return (Complete, False)
  where
    (!?) :: [Int] -> Int -> Maybe Int -- TODO: Can be taken to Kernel if used frequently
    xs !? n
      | n < 0 = Nothing
      | n >= length xs = Nothing
      | otherwise =
        foldr
          ( \x r k -> case k of
              0 -> Just x
              _ -> r (k -1)
          )
          (const Nothing)
          xs
          n
