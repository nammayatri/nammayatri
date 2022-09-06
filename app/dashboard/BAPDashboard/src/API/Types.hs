module API.Types where

import Data.Text (Text)
import Servant
import Tools.Auth
import Tools.Roles

type API =
  Get '[JSON] Text
    :<|> UserTestAPI
    :<|> AdminTestAPI
    :<|> JuspayOpsTestAPI

type UserTestAPI =
  "userTest"
    :> TokenAuth (AccessLevel 'READ_ACCESS 'CUSTOMERS)
    :> Get '[JSON] Text

type AdminTestAPI =
  "adminTest"
    :> TokenAuth (AccessLevel 'WRITE_ACCESS 'RIDES)
    :> Get '[JSON] Text

type JuspayOpsTestAPI =
  "adminTest"
    :> TokenAuth (AccessLevel 'WRITE_ACCESS 'MONITORING)
    :> Get '[JSON] Text
