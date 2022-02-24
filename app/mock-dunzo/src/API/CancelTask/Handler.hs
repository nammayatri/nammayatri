module API.CancelTask.Handler where

import qualified API.Cache as Cache
import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Cache as Cache
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
handler taskId mToken mClientId _mIsTestMode req = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  (requestId, taskStatus) <- Cache.findTaskById taskId
  canceledTask <- cancelTask taskStatus req
  Cache.setKey requestId canceledTask
  pure NoContent

cancelTask :: MonadTime m => API.TaskStatus -> API.CancelTaskReq -> m API.CreateTaskRes
cancelTask API.TaskStatus {..} (API.CancelTaskReq reason) = do
  now <- getCurrentTime
  pure
    API.TaskStatus
      { state = API.CANCELLED,
        request_timestamp = Just $ Time.timeToInt now,
        cancelled_by = Just "User",
        cancellation_reason = Just reason,
        eta = Nothing,
        ..
      }
