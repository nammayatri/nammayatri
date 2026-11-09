{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.VehicleTripOwner
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.VehicleTripOwner as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.VehicleTripOwner as VTO
import Storage.Beam.SystemConfigs ()

type API =
  "vehicleTripOwner"
    :> ( "resolve"
           :> Header "api-key" Text
           :> ReqBody '[JSON] Domain.VehicleTripOwnerReq
           :> Post '[JSON] VTO.VehicleTripOwner
           :<|> Capture "merchantId" (Id Merchant)
             :> "waybillChanged"
             :> Header "api-key" Text
             :> ReqBody '[JSON] Domain.WaybillChangedReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = resolve :<|> waybillChanged

resolve :: Maybe Text -> Domain.VehicleTripOwnerReq -> FlowHandler VTO.VehicleTripOwner
resolve apiKey = withFlowHandlerAPI . Domain.resolveVehicleTripOwner apiKey

waybillChanged :: Id Merchant -> Maybe Text -> Domain.WaybillChangedReq -> FlowHandler APISuccess
waybillChanged merchantId apiKey = withFlowHandlerAPI . Domain.waybillChanged merchantId apiKey
