module API.UI.Disability
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Disability as DDisability
import qualified Domain.Action.UI.Profile as PD
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "disability"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] [PD.DisabilityItem]
       )

handler :: FlowServer API
handler = listDisabilities

listDisabilities :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler [PD.DisabilityItem]
listDisabilities = withFlowHandlerAPI . DDisability.listDisabilities
