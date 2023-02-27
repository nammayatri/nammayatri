{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Transporter (module Reexport, API, handler) where

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
import Kernel.Types.Id
import Kernel.Utils.Common
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
