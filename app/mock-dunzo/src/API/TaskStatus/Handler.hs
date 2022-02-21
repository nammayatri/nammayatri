module API.TaskStatus.Handler where

import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import qualified Tools.Time as Time
import qualified "fmd-wrapper" Types.Common as Common

handler ::
  API.TaskId ->
  Maybe Common.Token ->
  Maybe Common.ClientId ->
  Maybe Bool ->
  FlowHandler API.TaskStatus
handler taskId mToken mClientId _mIsTestMode = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  cachedTasks <- findInCache (\taskStatus -> taskStatus.task_id == taskId)
  case cachedTasks of
    [(requestId, taskStatus)] -> do
      withCaching requestId $ updateTaskStatus taskStatus
    [] -> throwError (InvalidRequest "Task data not found")
    _ -> throwError (InternalError "More than one task with the same task_id found")

updateTaskStatus :: MonadTime m => API.TaskStatus -> m API.CreateTaskRes
updateTaskStatus API.TaskStatus {..} = do
  now <- getCurrentTime
  pure
    API.TaskStatus
      { eta = Just Fixtures.eta2,
        event_timestamp = Just $ Time.timeToInt now,
        ..
      }
