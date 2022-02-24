module API.Handler where

import qualified API.CancelTask.Handler as CancelTask
import qualified API.CreateTask.Handler as CreateTask
import qualified API.Quote.Handler as Quote
import qualified API.TaskStatus.Handler as TaskStatus
import qualified API.Token.Handler as Token
import qualified API.Types as API
import App.Types
import Servant

handler :: FlowServer API.API
handler =
  Token.handler
    :<|> Quote.handler
    :<|> CreateTask.handler
    :<|> TaskStatus.handler
    :<|> CancelTask.handler
