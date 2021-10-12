{-# LANGUAGE TypeApplications #-}

module Services.Allocation.Runner where

import App.BackgroundTaskManager.Types (DriverAllocationConfig)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Utils.Logging as Log
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Control.Monad.Catch (Handler (..), catches)
import qualified Data.Map as Map
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import qualified Services.Allocation.Internal as I
import Types.Error
import Types.Metrics (CoreMetrics, HasBTMMetrics)
import Types.Storage.Organization
import Utils.Common
import qualified Utils.Metrics as Metrics

handle ::
  ( Allocation.MonadHandler m,
    DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig],
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    HasBTMMetrics m r,
    CoreMetrics m
  ) =>
  Allocation.ServiceHandle m
handle =
  Allocation.ServiceHandle
    { getDriverSortMode = I.getDriverSortMode,
      getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getDriverBatchSize = I.getDriverBatchSize,
      getRequests = I.getRequests,
      getDriverPool = I.getDriverPool,
      getCurrentNotifications = I.getCurrentNotifications,
      cleanupOldNotifications = I.cleanupOldNotifications,
      sendNewRideNotifications = I.sendNewRideNotifications,
      sendRideNotAssignedNotification = I.sendRideNotAssignedNotification,
      addNotificationStatuses = I.addNotificationStatuses,
      updateNotificationStatuses = I.updateNotificationStatuses,
      resetLastRejectionTimes = I.resetLastRejectionTimes,
      getAttemptedDrivers = I.getAttemptedDrivers,
      getDriversWithNotification = I.getDriversWithNotification,
      getTopDriversByIdleTime = I.getTopDriversByIdleTime,
      checkAvailability = I.checkAvailability,
      assignDriver = I.assignDriver,
      cancelRide = I.cancelRide,
      addAllocationRequest = I.addAllocationRequest,
      getRideInfo = I.getRideInfo,
      cleanupNotifications = I.cleanupNotifications,
      removeRequest = I.removeRequest,
      logEvent = I.logEvent,
      logDriverEvents = I.logDriverEvents,
      metrics =
        Allocation.BTMMetricsHandle
          { incrementTaskCounter = Metrics.incrementTaskCounter,
            incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter,
            putTaskDuration = Metrics.putTaskDuration
          }
    }

getOrganizationLock ::
  HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig] => m (ShortId Organization)
getOrganizationLock = do
  shardMap <- asks (.driverAllocationConfig.shards)
  let numShards = Map.size shardMap
  shardCounter <- Redis.incrementKeyRedis "beckn:allocation:shardCounter"
  let shardId = fromIntegral $ abs $ shardCounter `rem` fromIntegral numShards
  case Map.lookup shardId shardMap of
    Just shortOrgId -> do
      lockAvailable <- Redis.tryLockRedis ("beckn:allocation:lock_" <> getShortId shortOrgId) 10
      logInfo ("Lock available on " <> getShortId shortOrgId <> ": " <> show lockAvailable)
      if lockAvailable
        then pure shortOrgId
        else getOrganizationLock
    Nothing ->
      throwError $
        ShardMappingError $ "Shard " <> show shardId <> " does not have an associated organization."

run ::
  ( HasBTMMetrics m r,
    DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["driverAllocationConfig" ::: DriverAllocationConfig],
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    HasFlowEnv m r '["isShuttingDown" ::: TMVar ()],
    CoreMetrics m
  ) =>
  m ()
run = do
  runnerHandler . withLogTag "Allocation service" $ do
    shortOrgId <- getOrganizationLock
    now <- getCurrentTime
    Redis.setKeyRedis "beckn:allocation:service" now
    ((), processTime) <- measureDuration $ do
      requestsNum <- asks (.driverAllocationConfig.requestsNumPerIteration)
      eres <- try $ Allocation.process handle shortOrgId requestsNum
      whenLeft eres $ Log.logError . show @_ @SomeException
      Redis.unlockRedis $ "beckn:allocation:lock_" <> getShortId shortOrgId
    -- If process handling took less than processDelay we delay for remain to processDelay time
    processDelay <- asks (.driverAllocationConfig.processDelay)
    liftIO . threadDelay . max 0 . getMicroseconds $ millisecondsToMicroseconds (processDelay - processTime)
  isRunning <- liftIO . atomically . isEmptyTMVar =<< asks (.isShuttingDown)
  when isRunning run
  where
    runnerHandler =
      flip
        catches
        [ Handler $ \(RedisError err) -> do
            Log.logTagError "Allocation service" $ show err
            liftIO $ threadDelay 1000000,
          Handler $ \(e :: SomeException) -> Log.logTagError "Allocation service" $ makeLogSomeException e
        ]
