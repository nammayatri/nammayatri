module API.UI.Transporter
  ( API,
    handler,
    DTransporter.TransporterRec (..),
    DTransporter.UpdateTransporterReq (..),
    DTransporter.UpdateTransporterRes,
  )
where

import Beckn.Types.Id (Id (..))
import Beckn.Utils.Common
import qualified Domain.Action.UI.Transporter as DTransporter
import qualified Domain.Types.Organization as SO
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth

-- Following is organization creation
type API =
  "transporter"
    :> ( TokenAuth
           :> Get '[JSON] DTransporter.TransporterRec
           :<|> AdminTokenAuth
           :> Capture "orgId" (Id SO.Organization)
           :> ReqBody '[JSON] DTransporter.UpdateTransporterReq
           :> Post '[JSON] DTransporter.UpdateTransporterRes
       )

handler :: FlowServer API
handler =
  getTransporter
    :<|> updateTransporter

updateTransporter :: SP.Person -> Id SO.Organization -> DTransporter.UpdateTransporterReq -> FlowHandler DTransporter.UpdateTransporterRes
updateTransporter admin orgId = withFlowHandlerAPI . DTransporter.updateTransporter admin orgId

getTransporter :: Id SP.Person -> FlowHandler DTransporter.TransporterRec
getTransporter = withFlowHandlerAPI . DTransporter.getTransporter
