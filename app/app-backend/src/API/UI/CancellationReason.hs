module API.UI.CancellationReason
  ( API,
    handler,
    ListRes,
  )
where

import Beckn.Types.Id
import qualified Domain.Action.UI.CancellationReason as DCancellationReason
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

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
