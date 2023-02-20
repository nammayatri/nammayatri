module API.Dashboard.Driver where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)

type API =
  "driver"
    :> ( Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriverAPI
           :<|> Common.DisableDriverAPI
           :<|> Common.BlockDriverAPI
           :<|> Common.UnblockDriverAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.UpdatePhoneNumberAPI
           :<|> Common.AddVehicleAPI
           :<|> Common.UpdateDriverNameAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDrivers merchantId
    :<|> driverActivity merchantId
    :<|> enableDriver merchantId
    :<|> disableDriver merchantId
    :<|> blockDriver merchantId
    :<|> unblockDriver merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> updatePhoneNumber merchantId
    :<|> addVehicle merchantId
    :<|> updateDriverName merchantId

listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset verified enabled blocked =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId mbLimit mbOffset verified enabled blocked

driverActivity :: ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI . DDriver.driverActivity

enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId = withFlowHandlerAPI . DDriver.enableDriver merchantShortId

disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId = withFlowHandlerAPI . DDriver.disableDriver merchantShortId

blockDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
blockDriver merchantShortId = withFlowHandlerAPI . DDriver.blockDriver merchantShortId

unblockDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unblockDriver merchantShortId = withFlowHandlerAPI . DDriver.unblockDriver merchantShortId

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber = withFlowHandlerAPI . DDriver.driverInfo merchantShortId mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber

deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId

unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId

updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId driverId

addVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId driverId

updateDriverName :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdateDriverNameReq -> FlowHandler APISuccess
updateDriverName merchantShortId driverId = withFlowHandlerAPI . DDriver.updateDriverName merchantShortId driverId
