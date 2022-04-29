module API.Types where

import Beckn.Types.APISuccess
import Data.Text (Text)
import Servant

type API = HelloAPI :<|> CreateJobAPI

type HelloAPI = Get '[JSON] Text

type CreateJobAPI =
  "job"
    :> ReqBody '[JSON] Text
    :> Get '[JSON] APISuccess
