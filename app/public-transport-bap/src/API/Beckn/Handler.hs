module API.Beckn.Handler where

import qualified API.Beckn.OnConfirm.Handler as OnConfirm
import qualified API.Beckn.OnSearch.Handler as OnSearch
import qualified API.Beckn.OnStatus.Handler as OnStatus
import qualified API.Beckn.Types as Beckn
import Environment
import Servant

handler :: FlowServer Beckn.API
handler auth =
  OnSearch.handler auth
    :<|> OnConfirm.handler auth
    :<|> OnStatus.handler auth
