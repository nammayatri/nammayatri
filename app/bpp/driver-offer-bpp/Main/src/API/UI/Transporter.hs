module API.UI.Transporter
  ( API,
    handler,
    DTransporter.TransporterRec (..),
    DTransporter.UpdateTransporterReq (..),
    DTransporter.UpdateTransporterRes,
  )
where

import qualified Domain.Action.UI.Transporter as DTransporter
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Servant
import Tools.Auth

-- Following is organization creation
type API =
  "transporter"
    :> ( TokenAuth
           :> Get '[JSON] DTransporter.TransporterRec
           :<|> AdminTokenAuth
           :> Capture "merchantId" (Id DM.Merchant)
           :> ReqBody '[JSON] DTransporter.UpdateTransporterReq
           :> Post '[JSON] DTransporter.UpdateTransporterRes
       )

handler :: FlowServer API
handler =
  getTransporter
    :<|> updateTransporter

updateTransporter :: SP.Person -> Id DM.Merchant -> DTransporter.UpdateTransporterReq -> FlowHandler DTransporter.UpdateTransporterRes
updateTransporter admin merchantId = withFlowHandlerAPI . DTransporter.updateTransporter admin merchantId

getTransporter :: Id SP.Person -> FlowHandler DTransporter.TransporterRec
getTransporter = withFlowHandlerAPI . DTransporter.getTransporter
