{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Overlay where

import qualified Domain.Action.Dashboard.Overlay as DOverlay
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

data OverlayEndpoint
  = CreateOverlayEndpoint
  | DeleteOverlayEndpoint
  | ScheduleOverlayEndpoint
  deriving (Show, Read)

derivePersistField "OverlayEndpoint"

type API =
  "overlay"
    :> ( CreateOverlayAPI
           :<|> DeleteOverlayAPI
           :<|> ListOverlayAPI
           :<|> OverlayInfoAPI
           :<|> ScheduleOverlayAPI
       )

type CreateOverlayAPI =
  "createOverlay"
    :> ReqBody '[JSON] DOverlay.CreateOverlayReq
    :> Post '[JSON] APISuccess

type DeleteOverlayAPI =
  "deleteOverlay"
    :> ReqBody '[JSON] DOverlay.DeleteOverlayReq
    :> Post '[JSON] APISuccess

type ListOverlayAPI =
  "listOverlay"
    :> Get '[JSON] DOverlay.ListOverlayResp

type OverlayInfoAPI =
  "overlayInfo"
    :> ReqBody '[JSON] DOverlay.OverlayInfoReq
    :> Get '[JSON] DOverlay.OverlayInfoResp

type ScheduleOverlayAPI =
  "scheduleOverlay"
    :> ReqBody '[JSON] DOverlay.ScheduleOverlay
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  createOverlay merchantId
    :<|> deleteOverlay merchantId
    :<|> listOverlay merchantId
    :<|> overlayInfo merchantId
    :<|> scheduleOverlay merchantId

createOverlay :: ShortId DM.Merchant -> DOverlay.CreateOverlayReq -> FlowHandler APISuccess
createOverlay merchantShortId req = withFlowHandlerAPI $ DOverlay.createOverlay merchantShortId req

deleteOverlay :: ShortId DM.Merchant -> DOverlay.DeleteOverlayReq -> FlowHandler APISuccess
deleteOverlay merchantShortId req = withFlowHandlerAPI $ DOverlay.deleteOverlay merchantShortId req

listOverlay :: ShortId DM.Merchant -> FlowHandler DOverlay.ListOverlayResp
listOverlay = withFlowHandlerAPI . DOverlay.listOverlay

overlayInfo :: ShortId DM.Merchant -> DOverlay.OverlayInfoReq -> FlowHandler DOverlay.OverlayInfoResp
overlayInfo merchantShortId = withFlowHandlerAPI . DOverlay.overlayInfo merchantShortId

scheduleOverlay :: ShortId DM.Merchant -> DOverlay.ScheduleOverlay -> FlowHandler APISuccess
scheduleOverlay merchantShortId = withFlowHandlerAPI . DOverlay.scheduleOverlay merchantShortId
