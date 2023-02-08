module API.UI.CancellationReason
  ( API,
    handler,
    ListRes,
  )
where

import qualified Domain.Action.UI.CancellationReason as DCancellationReason
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> MandatoryQueryParam "cancellationStage" DCR.CancellationStage
           :> Get '[JSON] ListRes
       )

type ListRes = [DCR.CancellationReasonAPIEntity]

handler :: FlowServer API
handler = list

list :: Id Person.Person -> DCR.CancellationStage -> FlowHandler ListRes
list _ = withFlowHandlerAPI . DCancellationReason.list
