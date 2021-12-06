module API.API where

import Servant
import Tools.Auth

type API =
  "testAuth"
    :> TokenAuth
    :> Get '[JSON] ()
