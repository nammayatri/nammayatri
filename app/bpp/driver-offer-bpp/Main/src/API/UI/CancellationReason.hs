module API.UI.CancellationReason
  ( CancellationReasonListRes,
    API,
    handler,
  )
where

import qualified Domain.Action.UI.CancellationReason as DCancellationReason
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] CancellationReasonListRes
       )

handler :: FlowServer API
handler = list

type CancellationReasonListRes = [SCR.CancellationReasonAPIEntity]

list :: Id Person.Person -> FlowHandler CancellationReasonListRes
list _ = withFlowHandlerAPI DCancellationReason.list
