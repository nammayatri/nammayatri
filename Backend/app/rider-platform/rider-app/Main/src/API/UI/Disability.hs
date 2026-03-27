module API.UI.Disability
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Disability as DDisability
import qualified Domain.Types.Disability as Disability
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "disability"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] [Disability.Disability]
       )

handler :: FlowServer API
handler = listDisabilities

listDisabilities :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler [Disability.Disability]
listDisabilities (personId, merchantId) = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ DDisability.listDisabilities (personId, merchantId)
