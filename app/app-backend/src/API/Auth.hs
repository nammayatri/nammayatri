module API.Auth
  ( API,
    handler,
  )
where

import App.Types
import Beckn.InternalAPI.Auth.API
import Beckn.Utils.Common
import qualified Domain.Action.Auth as DAuth
import EulerHS.Prelude

handler :: FlowServer API
handler = auth

auth :: Token -> FlowHandler PersonId
auth = withFlowHandlerAPI . DAuth.auth
