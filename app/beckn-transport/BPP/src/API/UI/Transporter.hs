module API.UI.Transporter (module Reexport, API, handler) where

import App.Types
import Beckn.Types.Id
import Domain.Action.UI.Transporter as Reexport
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
  )
import qualified Domain.Action.UI.Transporter as DTransp
import Domain.Types.Organization (Organization)
import qualified Domain.Types.Organization as SO
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth (AdminTokenAuth, TokenAuth)
import Utils.Common

type API =
  "transporter"
    :> ( TokenAuth
           :> Get '[JSON] TransporterRec
           :<|> AdminTokenAuth
           :> Capture "orgId" (Id Organization)
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] UpdateTransporterRes
       )

handler :: FlowServer API
handler =
  getTransporter
    :<|> updateTransporter

updateTransporter :: SP.Person -> Id SO.Organization -> UpdateTransporterReq -> FlowHandler UpdateTransporterRes
updateTransporter admin orgId = withFlowHandlerAPI . DTransp.updateTransporter admin orgId

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter = withFlowHandlerAPI . DTransp.getTransporter
