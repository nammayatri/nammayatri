module API.UI.CancellationReason (module Reexport, API, handler) where

import App.Types
import Beckn.Types.Id
import Domain.Action.UI.CancellationReason as Reexport
  ( ListRes,
  )
import qualified Domain.Action.UI.CancellationReason as DCR
import qualified Domain.Types.Person as Person
import Servant
import Utils.Auth
import Utils.Common

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
