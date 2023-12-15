{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Handler where

import qualified API.Types as API
import App.Scheduler
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Utils.Common
import Servant

handler :: FlowServer API.API
handler = helloHandler :<|> createJobHandler

helloHandler :: FlowHandler Text
helloHandler = withFlowHandlerAPI' $ pure "Hello, world!"

createJobHandler :: Text -> FlowHandler APISuccess
createJobHandler jobType = withFlowHandlerAPI' $ do
  case jobType of
    "bananas" -> void $ createBananasCountingJob 7
    "failing_time" -> void $ createTimePrinterJob 7
    "incorrect_data" -> void $ createIncorrectDataJob 7
    "fake_job" -> void $ createFakeJob 7
    "test_termination" -> void $ createTestTerminationJob 5
    _ -> logWarning "unknown job type"
  pure Success
