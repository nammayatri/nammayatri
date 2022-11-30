module API.Dashboard.Driver where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (Unauthorized, throwError)

type API =
  "driver"
    :> ( Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriverAPI
           :<|> Common.DisableDriverAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.UpdatePhoneNumberAPI
           :<|> Common.AddVehicleAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDrivers merchantId
    :<|> driverActivity merchantId
    :<|> enableDriver merchantId
    :<|> disableDriver merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> updatePhoneNumber merchantId
    :<|> addVehicle merchantId

listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset verified enabled =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId mbLimit mbOffset verified enabled

driverActivity :: ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI . DDriver.driverActivity

enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
enableDriver merchantShortId = withFlowHandlerAPI . DDriver.enableDriver merchantShortId

disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
disableDriver merchantShortId = withFlowHandlerAPI . DDriver.disableDriver merchantShortId

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset = withFlowHandlerAPI . DDriver.driverLocation merchantShortId mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber = withFlowHandlerAPI . DDriver.driverInfo merchantShortId mbMobileNumber

deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId = withFlowHandlerAPI . DDriver.deleteDriver merchantShortId

unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId = withFlowHandlerAPI . DDriver.unlinkVehicle merchantShortId

updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId driverId = withFlowHandlerAPI . DDriver.updatePhoneNumber merchantShortId driverId

addVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Common.AddVehicleReq -> FlowHandler APISuccess
addVehicle merchantShortId driverId = withFlowHandlerAPI . DDriver.addVehicle merchantShortId driverId
