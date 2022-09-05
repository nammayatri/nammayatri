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
    :> TokenAuth USER
    :> Get '[JSON] Text

type AdminTestAPI =
  "adminTest"
    :> TokenAuth ADMIN
    :> Get '[JSON] Text

type JuspayOpsTestAPI =
  "adminTest"
    :> TokenAuth JUSPAY_OPS
    :> Get '[JSON] Text
