module API.Types where

import qualified "fmd-wrapper" ExternalAPI.Dunzo.Flow as API
import Servant

type API =
  API.GetTokenAPI
    :<|> API.QuoteAPI
    :<|> API.CreateTaskAPI
    :<|> API.TaskStatusAPI
    :<|> API.CancelTaskAPI
