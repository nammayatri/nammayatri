module API.Types where

import Data.Text (Text)
import Kernel.Types.APISuccess
import Servant

type API = HelloAPI :<|> CreateJobAPI

type HelloAPI = Get '[JSON] Text

type CreateJobAPI =
  "job"
    :> ReqBody '[JSON] Text
    :> Get '[JSON] APISuccess
