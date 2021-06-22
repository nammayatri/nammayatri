module Services.Allocation.Runner where

import App.BackgroundTaskManager.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Types.Error.API (RedisError (..))
import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import qualified Beckn.Utils.Logging as Log
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Control.Monad.Catch (Handler (..), catches)
import qualified Data.Map as Map
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import qualified Services.Allocation.Internal as I
import Types.ShardMappingError
import Utils.Common

handle :: Allocation.ServiceHandle Flow
handle =
  Allocation.ServiceHandle
    { getDriverSortMode = I.getDriverSortMode,
      getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getRequests = I.getRequests,
      getDriverPool = I.getDriverPool,
      getCurrentNotification = I.getCurrentNotification,
      cleanupOldNotifications = I.cleanupOldNotifications,
      sendNewRideNotification = I.sendNewRideNotification,
      sendRideNotAssignedNotification = I.sendRideNotAssignedNotification,
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
      addAllocationRequest = I.addAllocationRequest,
      getRideInfo = I.getRideInfo,
      cleanupNotifications = I.cleanupNotifications,
      removeRequest = I.removeRequest,
      logEvent = I.logEvent
    }

getOrganizationLock :: Flow (ShortId Organization)
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
      L.throwException $
        ShardMappingError $ "Shard " <> show shardId <> " does not have an associated organization."

run :: Flow ()
run = do
  runnerHandler $ do
    shortOrgId <- getOrganizationLock
    now <- getCurrentTime
    Redis.setKeyRedis "beckn:allocation:service" now
    processStartTime <- getCurrentTime
    requestsNum <- asks (.driverAllocationConfig.requestsNumPerIteration)
    eres <- runSafeFlow $ Allocation.process handle shortOrgId requestsNum
    whenLeft eres $ Log.logTagError "Allocation service"
    Redis.unlockRedis $ "beckn:allocation:lock_" <> getShortId shortOrgId
    processEndTime <- getCurrentTime
    let processTime = diffUTCTime processEndTime processStartTime
    -- If process handling took less than processDelay we delay for remain to processDelay time
    processDelay <- asks (.driverAllocationConfig.processDelay)
    L.runIO $ threadDelay $ fromNominalToMicroseconds $ max 0 (processDelay - processTime)
  isRunning <- L.runIO . liftIO . atomically . isEmptyTMVar =<< asks (.appEnv.isShuttingDown)
  when isRunning run
  where
    fromNominalToMicroseconds = floor . (1000000 *) . nominalDiffTimeToSeconds
    runnerHandler =
      flip
        catches
        [ Handler $ \(RedisError err) -> do
            Log.logTagError "Allocation service" $ show err
            L.runIO $ threadDelay 1000000,
          Handler $ \(APIException e) -> Log.logTagError "Allocation service" $ toLogMessageAPIError e,
          Handler $ \(e :: SomeException) -> Log.logTagError "Allocation service" $ show e
        ]
