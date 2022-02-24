module API.TaskStatus.Handler where

import qualified API.Cache as Cache
import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Cache as Cache
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
  (requestId, taskStatus) <- Cache.findTaskById taskId
  updatedTask <- updateTaskStatus taskStatus
  Cache.setKey requestId updatedTask
  pure updatedTask

updateTaskStatus :: MonadTime m => API.TaskStatus -> m API.CreateTaskRes
updateTaskStatus API.TaskStatus {..} = do
  now <- getCurrentTime
  pure
    API.TaskStatus
      { eta = if state == API.CANCELLED then Nothing else Just Fixtures.eta2,
        request_timestamp = Just $ Time.timeToInt now,
        estimated_price = Nothing,
        ..
      }
