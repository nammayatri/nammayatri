module API.CreateTask.Handler where

import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Cache as Cache
import Beckn.Types.Error
import Beckn.Utils.Common
import ExternalAPI.Dunzo.Types (TaskStatus (task_id))
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import qualified Tools.Time as Time
import qualified "fmd-wrapper" Types.Common as Common

handler ::
  Maybe Common.Token ->
  Maybe Common.ClientId ->
  Maybe Bool ->
  API.CreateTaskReq ->
  FlowHandler API.CreateTaskRes
handler mToken mClientId _mIsTestMode req = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  mTaskStatus <- Cache.getKey req.request_id
  whenJust (mTaskStatus :: Maybe API.TaskStatus) $
    \_ -> throwError (InvalidRequest "Request with same request id has already been processed")
  taskStatus <- buildCreateTaskRes
  Cache.setKey req.request_id taskStatus
  Cache.setKey taskStatus.task_id req.request_id
  pure taskStatus

buildCreateTaskRes :: (MonadGuid m, MonadTime m) => m API.CreateTaskRes
buildCreateTaskRes = do
  id <- generateGUID
  now <- getCurrentTime
  pure
    API.TaskStatus
      { task_id = API.TaskId id,
        state = API.CREATED,
        eta = Just Fixtures.eta,
        estimated_price = Just 79.0,
        event_timestamp = Nothing,
        request_timestamp = Just $ Time.timeToInt now,
        tracking_url = Nothing,
        runner = Nothing,
        price = Nothing,
        total_time = Nothing,
        cancelled_by = Nothing,
        cancellation_reason = Nothing
      }
