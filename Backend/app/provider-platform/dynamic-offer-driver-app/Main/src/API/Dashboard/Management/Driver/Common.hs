{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Management.Driver.Common where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "driver"
    :> ( Common.DriverDocumentsInfoAPI
           :<|> Common.DriverAadhaarInfoAPI
           :<|> Common.DriverAadhaarInfoByPhoneAPI
           :<|> Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.DisableDriverAPI
           :<|> BlockDriverWithReasonAPI
           :<|> Common.BlockDriverAPI
           :<|> Common.DriverBlockReasonListAPI
           :<|> UnblockDriverAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkDLAPI
           :<|> Common.UnlinkAadhaarAPI
           :<|> Common.UpdatePhoneNumberAPI
           :<|> Common.UpdateDriverAadhaarAPI
           :<|> Common.UpdateDriverNameAPI
           :<|> Common.DeleteRCAPI
           :<|> Common.ClearOnRideStuckDriversAPI
       )

type BlockDriverWithReasonAPI =
  Capture "driverId" (Id Common.Driver)
    :> "blockWithReason"
    :> Capture "dashboardUserName" Text
    :> ReqBody '[JSON] Common.BlockDriverWithReasonReq
    :> Post '[JSON] APISuccess

type UnblockDriverAPI =
  Capture "driverId" (Id Common.Driver)
    :> "unblock"
    :> Capture "dashboardUserName" Text
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  driverDocumentsInfo merchantId city
    :<|> driverAadhaarInfo merchantId city
    :<|> driverAadhaarInfoByPhone merchantId city
    :<|> listDrivers merchantId city
    :<|> driverActivity merchantId city
    :<|> disableDriver merchantId city
    :<|> blockDriverWithReason merchantId city
    :<|> blockDriver merchantId city
    :<|> blockReasonList merchantId city
    :<|> unblockDriver merchantId city
    :<|> driverLocation merchantId city
    :<|> deleteDriver merchantId city
    :<|> unlinkDL merchantId city
    :<|> unlinkAadhaar merchantId city
    :<|> updatePhoneNumber merchantId city
    :<|> updateByPhoneNumber merchantId city
    :<|> updateDriverName merchantId city
    :<|> deleteRC merchantId city
    :<|> clearOnRideStuckDrivers merchantId city

driverDocumentsInfo :: ShortId DM.Merchant -> Context.City -> FlowHandler Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId = withFlowHandlerAPI . DDriver.driverDocumentsInfo merchantShortId

driverAadhaarInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.DriverAadhaarInfoRes
driverAadhaarInfo merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfo merchantShortId opCity

driverAadhaarInfoByPhone :: ShortId DM.Merchant -> Context.City -> Text -> FlowHandler Common.DriverAadhaarInfoByPhoneReq
driverAadhaarInfoByPhone merchantShortId opCity = withFlowHandlerAPI . DDriver.driverAadhaarInfoByPhone merchantShortId opCity

listDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId opCity mbLimit mbOffset verified enabled blocked mbSubscribed vechicleNumberSearchString

driverActivity :: ShortId DM.Merchant -> Context.City -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId = withFlowHandlerAPI . DDriver.driverActivity merchantShortId

disableDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.disableDriver merchantShortId opCity

blockDriverWithReason :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> Common.BlockDriverWithReasonReq -> FlowHandler APISuccess
blockDriverWithReason merchantShortId opCity driverId dashboardUserName = withFlowHandlerAPI . DDriver.blockDriverWithReason merchantShortId opCity driverId dashboardUserName

blockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.blockDriver merchantShortId opCity

blockReasonList :: ShortId DM.Merchant -> Context.City -> FlowHandler [Common.BlockReason]
blockReasonList _ _ = withFlowHandlerAPI DDriver.blockReasonList

unblockDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Text -> FlowHandler APISuccess
unblockDriver merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.unblockDriver merchantShortId opCity driverId

driverLocation :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId opCity mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId opCity mbLimit mbOffset

deleteDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId opCity = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId opCity

unlinkDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkDL merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkDL merchantShortId opCity

unlinkAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler APISuccess
unlinkAadhaar merchantShortId opCity = withFlowHandlerAPI . DDriver.unlinkAadhaar merchantShortId opCity

updatePhoneNumber :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId opCity driverId

updateByPhoneNumber :: ShortId DM.Merchant -> Context.City -> Text -> Common.UpdateDriverDataReq -> FlowHandler APISuccess
updateByPhoneNumber merchantShortId opCity mobileNo = withFlowHandlerAPI . DDriver.updateByPhoneNumber merchantShortId opCity mobileNo

updateDriverName :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.updateDriverName merchantShortId opCity driverId

deleteRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DeleteRCReq -> FlowHandler APISuccess
deleteRC merchantShortId opCity driverId = withFlowHandlerAPI . DDriver.deleteRC merchantShortId opCity driverId

clearOnRideStuckDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> FlowHandler Common.ClearOnRideStuckDriversRes
clearOnRideStuckDrivers merchantShortId opCity = withFlowHandlerAPI . DDriver.clearOnRideStuckDrivers merchantShortId opCity
