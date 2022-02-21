module API.Types where

import Data.Text (Text)
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Flow as API
import Servant

type API =
  Get '[JSON] Text
    :<|> API.GetTokenAPI
    :<|> API.QuoteAPI
    :<|> API.CreateTaskAPI
    :<|> API.TaskStatusAPI
    :<|> API.CancelTaskAPI
