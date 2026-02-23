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

import qualified Data.HashMap.Strict as HM
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Scheduler.Types (ExecutionResult (..))
import SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.DriverPool

type HandleMonad m r = (MonadClock m, Log m, CoreMetrics m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal], HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl], HasRequestId r)

data MetricsHandle m = MetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m ()
  }

data Handle m r = Handle
  { isBatchNumExceedLimit :: m Bool,
    isReceivedMaxDriverQuotes :: m Bool,
    getNextDriverPoolBatch :: GoHomeConfig -> m DriverPoolWithActualDistResultWithFlags,
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> [Id Driver] -> GoHomeConfig -> m (),
    getRescheduleTime :: m UTCTime,
    metrics :: MetricsHandle m,
    isSearchTryValid :: m Bool,
    isBookingValid :: Bool,
    initiateDriverSearchBatch :: m DST.SearchTry,
    cancelSearchTry :: m (),
    cancelBookingIfApplies :: m (),
    isScheduledBooking :: Bool
  }

handler :: HandleMonad m r => Handle m r -> GoHomeConfig -> Text -> m (ExecutionResult, PoolType, Maybe Seconds)
handler h@Handle {..} goHomeCfg transactionId = do
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
          else processRequestSending h goHomeCfg transactionId

processRequestSending :: HandleMonad m r => Handle m r -> GoHomeConfig -> Text -> m (ExecutionResult, PoolType, Maybe Seconds)
processRequestSending Handle {..} goHomeCfg transactionId = do
  isBatchNumExceedLimit' <- isBatchNumExceedLimit
  logInfo $ "processRequestSending isBatchNumExceedLimit: " <> show isBatchNumExceedLimit'
  if isBatchNumExceedLimit'
    then do
      if isScheduledBooking
        then do
          void initiateDriverSearchBatch
          return (Complete, NormalPool, Nothing)
        else do
          metrics.incrementFailedTaskCounter
          logInfo "No driver accepted"
          appBackendBapInternal <- asks (.appBackendBapInternal)
          let request = CallBAPInternal.RideSearchExpiredReq {transactionId = transactionId}
          void $ CallBAPInternal.rideSearchExpired appBackendBapInternal.apiKey appBackendBapInternal.url request
          cancelSearchTry
          cancelBookingIfApplies
          return (Complete, NormalPool, Nothing)
    else do
      driverPoolWithFlags <- getNextDriverPoolBatch goHomeCfg
      when (not $ null driverPoolWithFlags.driverPoolWithActualDistResult) $
        sendSearchRequestToDrivers driverPoolWithFlags.driverPoolWithActualDistResult driverPoolWithFlags.prevBatchDrivers goHomeCfg
      ReSchedule <$> getRescheduleTime <&> (,driverPoolWithFlags.poolType,driverPoolWithFlags.nextScheduleTime)
