module Services.Allocation.Runner where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
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
      _ <- Allocation.process handle
      Redis.setExRedis "beckn:allocation:is_running" False 60
      L.runIO $ atomically $ takeTMVar activeTask
      L.runIO $ threadDelay 5000000 -- Adding delay to limit number of getTasks requests to DB per sec. TODO: modify with better impl.
  isRunning <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  when isRunning $ run shutdown activeTask
