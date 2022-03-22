module Services.Allocation.Runner where

import App.Allocator.Environment
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Utils.Logging as Log
import Beckn.Utils.Shutdown
import Control.Monad.Catch (Handler (..), catches)
import qualified Data.Map as Map
import Domain.Types.Organization
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import qualified Services.Allocation.Internal as I
import qualified Tools.Metrics as Metrics
import qualified Tools.Metrics as TMetrics
import Types.Error
import Utils.Common

handle :: Allocation.ServiceHandle Flow
handle =
  Allocation.ServiceHandle
    { getDriverSortMode = I.getDriverSortMode,
      getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getConfiguredReallocationsLimit = I.getConfiguredReallocationsLimit,
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
      cancelRideBooking = I.cancelRideBooking,
      addAllocationRequest = I.addAllocationRequest,
      getRideInfo = I.getRideInfo,
      cleanupNotifications = I.cleanupNotifications,
      removeRequest = I.removeRequest,
      logEvent = I.logEvent,
      logDriverEvents = I.logDriverEvents,
      metrics =
        Allocation.AllocatorMetricsHandle
          { incrementTaskCounter = Metrics.incrementTaskCounter,
            incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter,
            putTaskDuration = Metrics.putTaskDuration,
            incrementErrorCounter = TMetrics.incrementErrorCounter "ALLOCATOR_ERROR"
          }
    }

getOrganizationLock :: Flow (ShortId Organization)
getOrganizationLock = do
  shardMap <- askConfig (.shards)
  let numShards = Map.size shardMap
  shardCounter <- Redis.incrementKeyRedis "beckn:allocation:shardCounter"
  let shardId = fromIntegral $ abs $ shardCounter `rem` fromIntegral numShards
  case Map.lookup shardId shardMap of
    Just shortOrgId -> do
      lockAvailable <- Redis.tryLockRedis ("beckn:allocation:lock_" <> getShortId shortOrgId) 10
      logInfo ("Counter: " <> show shardCounter <> " | Shard id: " <> show shardId <> " | Lock availability on " <> getShortId shortOrgId <> ": " <> show lockAvailable)
      if lockAvailable
        then pure shortOrgId
        else getOrganizationLock
    Nothing ->
      throwError $
        ShardMappingError $ "Shard " <> show shardId <> " does not have an associated organization."

run :: Flow ()
run = do
  untilShutdown . runnerHandler . withLogTag "Allocation service" $ do
    shortOrgId <- getOrganizationLock
    log INFO $ "Got lock for " <> shortOrgId.getShortId
    now <- getCurrentTime
    Redis.setKeyRedis "beckn:allocation:service" now
    ((), processTime) <- measureDuration $ do
      requestsNum <- askConfig (.requestsNumPerIteration)
      eres <- try $ Allocation.process handle shortOrgId requestsNum
      whenLeft eres $ Log.logError . show @_ @SomeException
      Redis.unlockRedis $ "beckn:allocation:lock_" <> getShortId shortOrgId
    -- If process handling took less than processDelay we delay for remain to processDelay time
    processDelay <- askConfig (.processDelay)
    liftIO . threadDelay . max 0 . getMicroseconds $ millisecondsToMicroseconds (processDelay - processTime)
  where
    runnerHandler =
      flip
        catches
        [ Handler $ \(RedisError err) -> do
            Log.logTagError "Allocation service" $ show err
            liftIO $ threadDelay 1000000,
          Handler $ \(e :: SomeException) -> Log.logTagError "Allocation service" $ makeLogSomeException e
        ]
