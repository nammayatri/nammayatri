module API.PublicTransport.Search.Types where

import Domain.Outcoming.Search
import Servant
import Tools.Auth

type API =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes
