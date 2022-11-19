module API.UI.Transporter (module Reexport, API, handler) where

import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Transporter as Reexport
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
  )
import qualified Domain.Action.UI.Transporter as DTransp
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth (AdminTokenAuth, TokenAuth)

type API =
  "transporter"
    :> ( TokenAuth
           :> Get '[JSON] TransporterRec
           :<|> AdminTokenAuth
           :> Capture "merchantId" (Id Merchant)
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] UpdateTransporterRes
       )

handler :: FlowServer API
handler =
  getTransporter
    :<|> updateTransporter

updateTransporter :: SP.Person -> Id DM.Merchant -> UpdateTransporterReq -> FlowHandler UpdateTransporterRes
updateTransporter admin merchantId = withFlowHandlerAPI . DTransp.updateTransporter admin merchantId

getTransporter :: Id SP.Person -> FlowHandler TransporterRec
getTransporter = withFlowHandlerAPI . DTransp.getTransporter
