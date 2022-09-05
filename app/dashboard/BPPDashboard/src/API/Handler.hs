module API.Handler where

import qualified API.Types as API
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person as DP
import Environment
import Servant

handler :: FlowServer API.API
handler =
  pure "Hello, world!"
    :<|> userTestHandler
    :<|> adminTestHandler
    :<|> juspayOpsTestHandler

userTestHandler :: Id DP.Person -> FlowHandler Text
userTestHandler _ = withFlowHandlerAPI $ do
  pure "Hello, user!"

adminTestHandler :: Id DP.Person -> FlowHandler Text
adminTestHandler _ = withFlowHandlerAPI $ do
  pure "Hello, admin!"

juspayOpsTestHandler :: Id DP.Person -> FlowHandler Text
juspayOpsTestHandler _ = withFlowHandlerAPI $ do
  pure "Hello, Juspay OPS!"
