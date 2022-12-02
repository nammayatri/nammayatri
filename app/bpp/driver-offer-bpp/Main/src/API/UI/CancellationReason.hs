module API.UI.CancellationReason
  ( CancellationReasonListRes,
    API,
    handler,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.CancellationReason as DCancellationReason
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import Environment
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
