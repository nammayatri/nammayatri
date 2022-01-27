module API.Beckn.Handler where

import qualified API.Beckn.OnConfirm.Handler as OnConfirm
import qualified API.Beckn.OnSearch.Handler as OnSearch
import qualified API.Beckn.Types as Beckn
import App.Types
import Servant

handler :: FlowServer Beckn.API
handler auth =
  OnSearch.publicTransportOnSearch auth :<|> OnConfirm.handler auth
