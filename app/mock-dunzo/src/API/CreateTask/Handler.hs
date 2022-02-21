module API.CreateTask.Handler where

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
  Maybe Common.Token ->
  Maybe Common.ClientId ->
  Maybe Bool ->
  API.CreateTaskReq ->
  FlowHandler API.CreateTaskRes
handler mToken mClientId _mIsTestMode req = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  mTaskStatus <- getKey req.request_id
  whenJust (mTaskStatus :: Maybe API.TaskStatus) $
    \_ -> throwError (InvalidRequest "Request with same request id has already been processed")
  withCaching req.request_id buildCreateTaskRes

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
