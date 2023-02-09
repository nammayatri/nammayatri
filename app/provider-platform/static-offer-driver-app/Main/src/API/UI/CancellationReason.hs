module API.UI.CancellationReason (module Reexport, API, handler) where

import Domain.Action.UI.CancellationReason as Reexport
  ( ListRes,
  )
import qualified Domain.Action.UI.CancellationReason as DCR
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
           :> Get '[JSON] ListRes
       )

handler :: FlowServer API
handler = list

list :: Id Person.Person -> FlowHandler ListRes
list _ = withFlowHandlerAPI DCR.list
