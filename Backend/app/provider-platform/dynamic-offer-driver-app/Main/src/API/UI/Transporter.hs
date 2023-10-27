{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
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

getTransporter :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DTransporter.TransporterRec
getTransporter = withFlowHandlerAPI . DTransporter.getTransporter
