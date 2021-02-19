{-# LANGUAGE TypeApplications #-}

module Services.Allocation.Runner where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import EulerHS.Language ()
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import qualified Services.Allocation.Internal as I

handle :: Allocation.ServiceHandle Flow
handle =
  Allocation.ServiceHandle
    { getCurrentTime = I.getCurrentTime,
      getDriverSortMode = I.getDriverSortMode,
      getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getRequests = I.getRequests,
      getDriverPool = I.getDriverPool,
      getCurrentNotification = I.getCurrentNotification,
      sendNotification = I.sendNotification,
      addNotificationStatus = I.addNotificationStatus,
      updateNotificationStatus = I.updateNotificationStatus,
      resetLastRejectionTime = I.resetLastRejectionTime,
      getAttemptedDrivers = I.getAttemptedDrivers,
      getDriversWithNotification = I.getDriversWithNotification,
      getFirstDriverInTheQueue = I.getFirstDriverInTheQueue,
      checkAvailability = I.checkAvailability,
      getDriverResponse = I.getDriverResponse,
      assignDriver = I.assignDriver,
      cancelRide = I.cancelRide,
      completeRequest = I.completeRequest,
      resetRequestTime = I.resetRequestTime,
      logInfo = I.logInfo
    }

run :: TMVar () -> TMVar () -> Flow ()
run shutdown activeTask = do
  Redis.getKeyRedis "beckn:allocation:is_running" >>= \case
    Just True -> L.runIO $ threadDelay 5000000 -- sleep for a bit
    _ -> do
      Redis.setExRedis "beckn:allocation:is_running" True 60
      now <- getCurrTime
      Redis.setKeyRedis "beckn:allocation:service" now
      L.runIO $ atomically $ putTMVar activeTask ()
      processStartTime <- getCurrTime
      eres <- runSafeFlow $ Allocation.process handle requestsPerIteration
      whenLeft eres $ L.logError @Text "Allocation service"
      Redis.setExRedis "beckn:allocation:is_running" False 60
      processEndTime <- getCurrTime
      let processTime = diffUTCTime processEndTime processStartTime
      let oneSec = secondsToNominalDiffTime 1
      L.runIO $ atomically $ takeTMVar activeTask
      -- If process handling took less than 1 second we delay for remain to 1 second time
      L.runIO $ threadDelay $ fromNominalToMicroseconds $ max 0 (oneSec - processTime)
  isRunning <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  when isRunning $ run shutdown activeTask
  where
    requestsPerIteration = 50
    fromNominalToMicroseconds = floor . (1000000 *) . nominalDiffTimeToSeconds
