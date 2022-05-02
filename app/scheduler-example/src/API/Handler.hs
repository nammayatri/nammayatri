module API.Handler where

import qualified API.Types as API
import App.Scheduler
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Utils.Common
import Environment
import Servant

handler :: FlowServer API.API
handler = helloHandler :<|> createJobHandler

helloHandler :: FlowHandler Text
helloHandler = withFlowHandlerAPI $ pure "Hello, world!"

createJobHandler :: Text -> FlowHandler APISuccess
createJobHandler jobType = withFlowHandlerAPI $ do
  case jobType of
    "bananas" -> void $ createBananasCountingJob 7
    "failing_time" -> void $ createTimePrinterJob 7
    "incorrect_data" -> void $ createIncorrectDataJob 7
    "fake_job" -> void $ createFakeJob 7
    "test_termination" -> void $ createTestTerminationJob 5
    _ -> logWarning "unknown job type"
  pure Success
