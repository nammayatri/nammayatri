module API.CancelTask.Handler where

import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Types.Cache
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import Servant (NoContent (NoContent))
import qualified Tools.Time as Time
import qualified "fmd-wrapper" Types.Common as Common

handler ::
  API.TaskId ->
  Maybe Common.Token ->
  Maybe Common.ClientId ->
  Maybe Bool ->
  API.CancelTaskReq ->
  FlowHandler NoContent
handler taskId mToken mClientId _mIsTestMode _req = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  cachedTasks <- findInCache (\taskStatus -> taskStatus.task_id == taskId)
  (requestId, taskStatus) <- case cachedTasks of
    [(requestId, taskStatus)] -> pure (requestId, taskStatus)
    [] -> throwError (InvalidRequest "Task data not found")
    _ -> throwError (InternalError "More than one task with the same task_id found")
  _ <- withCaching requestId $ cancelTask taskStatus
  pure NoContent

cancelTask :: MonadTime m => API.TaskStatus -> m API.CreateTaskRes
cancelTask API.TaskStatus {..} = do
  now <- getCurrentTime
  pure
    API.TaskStatus
      { state = API.CANCELLED,
        event_timestamp = Just $ Time.timeToInt now,
        ..
      }
