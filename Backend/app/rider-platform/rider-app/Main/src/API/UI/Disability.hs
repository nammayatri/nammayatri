module API.UI.Disability
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Disability as DDisability
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDisability as PersonDisability
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "disability"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] [PersonDisability.DisabilityItem]
       )

handler :: FlowServer API
handler = listDisabilities

listDisabilities :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler [PersonDisability.DisabilityItem]
listDisabilities = withFlowHandlerAPI . DDisability.listDisabilities
