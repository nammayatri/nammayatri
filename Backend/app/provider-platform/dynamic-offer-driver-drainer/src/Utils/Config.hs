module Utils.Config where

import Constants as C
import Data.Text
import EulerHS.Prelude
import Types.Config
import Types.DBSync
import Utils.Redis

-- import Prelude

getDBSyncConfig :: Flow (Either Text DBSyncConfig)
getDBSyncConfig = runExceptT $ do
  retryMs <- ExceptT $ readHashKey C.dbsyncConfigKey "empty-retry-time"
  rateLimitN <- ExceptT $ readHashKey C.dbsyncConfigKey "rate-limit-n"
  rateLimitWindow <- ExceptT $ readHashKey C.dbsyncConfigKey "rate-limit-window"
  streamReadCount <- ExceptT $ readHashKey C.dbsyncConfigKey "stream-read-count"
  pure $
    DBSyncConfig
      { _emptyRetry = retryMs,
        _rateLimitN = rateLimitN,
        _rateLimitWindow = rateLimitWindow,
        _streamReadCount = streamReadCount
      }
