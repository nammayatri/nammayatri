{-# LANGUAGE TypeApplications #-}

module Services.Runner where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import qualified EulerHS.Language as L
import EulerHS.Prelude

-- import qualified Services.Runner.Allocation as Allocation

run :: TMVar () -> TMVar () -> Flow ()
run shutdown activeTask = do
  Redis.getKeyRedis "beckn:allocation:is_running" >>= \case
    Just True -> L.runIO $ threadDelay 5000000 -- sleep for a bit
    _ -> do
      Redis.setExRedis "beckn:allocation:is_running" True 60
      now <- getCurrTime
      Redis.setKeyRedis "beckn:allocation:service" now
      L.runIO $ atomically $ putTMVar activeTask ()
      L.logInfo @Text "Runner" "Start new iteration of task runner."
      -- _ <- Allocation.runAllocation
      Redis.setExRedis "beckn:allocation:is_running" False 60
      L.runIO $ atomically $ takeTMVar activeTask
      L.logInfo @Text "Runner" "Iteration of task runner is complete."
      L.runIO $ threadDelay 5000000 -- Adding delay to limit number of getTasks requests to DB per sec. TODO: modify with better impl.
  isRunning <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  when isRunning $ run shutdown activeTask
