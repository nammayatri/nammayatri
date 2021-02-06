{-# LANGUAGE TypeApplications #-}

module Services.Runner where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude

-- import qualified Services.Runner.Allocation as Allocation

run :: Flow ()
run = do
  now <- getCurrTime
  Redis.setKeyRedis "beckn:allocation:service" now
  -- _ <- Allocation.runAllocation
  L.logInfo @Text "Runner" $ "I'm alive"
  L.runIO $ threadDelay 100000
  run
