module API.Cache where

import qualified API.Error as Error
import App.Types
import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API

findTaskById ::
  ( Monad m,
    Cache API.TaskStatus m,
    Cache RequestId m,
    MonadThrow m,
    Log m
  ) =>
  API.TaskId ->
  m (RequestId, API.TaskStatus)
findTaskById taskId = do
  requestId <- getKey taskId >>= fromMaybeM Error.badRequest
  taskStatus <-
    getKey requestId
      >>= fromMaybeM (Error.internalError $ "taskStatus not found for requestId = " <> show requestId <> " and taskId = " <> show taskId)
  pure (requestId, taskStatus)
