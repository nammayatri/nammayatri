module API.Auth
  ( API,
    handler,
  )
where

import qualified Domain.Action.Auth as DAuth
import Environment
import EulerHS.Prelude
import Kernel.InternalAPI.Auth.API
import Kernel.Utils.Common

handler :: FlowServer API
handler = auth

auth :: Token -> FlowHandler PersonId
auth = withFlowHandlerAPI . DAuth.auth
