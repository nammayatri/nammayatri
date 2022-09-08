module API.BAP where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import Domain.Types.Person as DP
import Environment
import Servant
import Tools.Auth
import Tools.Roles.Instances

type API =
  "bap"
    :> "customer"
    :> "list"
    :> TokenAuth (ApiAccessLevel 'READ_ACCESS 'CUSTOMERS)
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver :: Id DP.Person -> FlowHandler Text
listDriver _ = withFlowHandlerAPI $ do
  pure "To be done"
