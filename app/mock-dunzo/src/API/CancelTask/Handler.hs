module API.CancelTask.Handler where

import qualified API.Cache as Cache
import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Cache as Cache
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
  (requestId, taskStatus) <- Cache.findTaskById taskId >>= fromMaybeM (InvalidRequest "Task data not found")
  canceledTask <- cancelTask taskStatus
  Cache.setKey requestId canceledTask
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
