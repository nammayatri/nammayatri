module API.Cache where

import App.Types
import Beckn.Prelude
import Beckn.Types.Cache
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API

findTaskById ::
  (Monad m, Cache API.TaskStatus m, Cache RequestId m) =>
  API.TaskId ->
  m (Maybe (RequestId, API.TaskStatus))
findTaskById taskId = do
  mbRequestId <- getKey taskId
  case mbRequestId of
    Nothing -> pure Nothing
    Just requestId -> do
      mbTaskStatus <- getKey requestId
      pure $ (,) requestId <$> mbTaskStatus
