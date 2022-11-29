module API.Dashboard.Driver where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (Unauthorized, throwError)

type API =
  "driver"
    :> ( Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriversAPI
           :<|> Common.DisableDriversAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Common.UnlinkVehicleAPI
           :<|> Common.UpdatePhoneNumberAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDrivers merchantId
    :<|> driverActivity merchantId
    :<|> enableDrivers merchantId
    :<|> disableDrivers merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> updatePhoneNumber merchantId

listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset verified enabled =
  withFlowHandlerAPI . DDriver.listDrivers merchantShortId mbLimit mbOffset verified enabled

driverActivity :: ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI . DDriver.driverActivity

enableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers merchantShortId = withFlowHandlerAPI . DDriver.enableDrivers merchantShortId

disableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers merchantShortId = withFlowHandlerAPI . DDriver.disableDrivers merchantShortId

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset = withFlowHandler . DDriver.driverLocation merchantShortId mbLimit mbOffset

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber = withFlowHandler . DDriver.driverInfo merchantShortId mbMobileNumber

deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId = withFlowHandler . DDriver.deleteDriver merchantShortId

unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle merchantShortId = withFlowHandler . DDriver.unlinkVehicle merchantShortId

updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber merchantShortId driverId = withFlowHandler . DDriver.updatePhoneNumber merchantShortId driverId
