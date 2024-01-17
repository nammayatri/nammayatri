{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Frontend where

import qualified Domain.Action.UI.Frontend as DFrontend
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

data RideNotifyEventEndPoint = NotifyEventEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "RideNotifyEventEndPoint"

type API =
  "frontend"
    :> ( PersonFlowStatusAPI
           :<|> NotifyEventAPI
       )

type PersonFlowStatusAPI =
  "flowStatus"
    :> Capture "customerId" (Id DP.Person)
    :> QueryParam "isPolling" Bool
    :> Get '[JSON] DFrontend.GetPersonFlowStatusRes

type NotifyEventAPI =
  "notifyEvent"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DFrontend.NotifyEventReq
    :> Post '[JSON] DFrontend.NotifyEventResp

handler :: FlowServer API
handler =
  callGetPersonFlowStatus
    :<|> callNotifyEvent

callGetPersonFlowStatus :: Id DP.Person -> Maybe Bool -> FlowHandler DFrontend.GetPersonFlowStatusRes
callGetPersonFlowStatus personId = withFlowHandlerAPI . DFrontend.getPersonFlowStatus personId

callNotifyEvent :: Id DP.Person -> DFrontend.NotifyEventReq -> FlowHandler DFrontend.NotifyEventResp
callNotifyEvent personId = withFlowHandlerAPI . DFrontend.notifyEvent personId
