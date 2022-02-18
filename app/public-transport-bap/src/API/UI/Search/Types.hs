module API.UI.Search.Types where

import qualified Domain.Endpoints.UI.Search as DSearch
import Servant
import Tools.Auth

type API =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] DSearch.SearchReq
    :> Post '[JSON] DSearch.SearchRes
