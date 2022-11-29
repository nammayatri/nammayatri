{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module API.BPP.BecknTransport.Driver
  ( API,
    handler,
  )
where

import qualified BPPClient.BecknTransport as Client
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common (throwError, withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "driver"
    :> ( DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> DeleteDriverAPI
           :<|> UnlinkVehicleAPI
           :<|> UpdatePhoneNumberAPI
       )

type DriverListAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverListAPI

type DriverActivityAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverActivityAPI

type EnableDriversAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.EnableDriversAPI

type DisableDriversAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.DisableDriversAPI

type DriverLocationAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverLocationAPI

type DriverInfoAPI =
  ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverInfoAPI

type DeleteDriverAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.DeleteDriverAPI

type UnlinkVehicleAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.UnlinkVehicleAPI

type UpdatePhoneNumberAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.UpdatePhoneNumberAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDriver merchantId
    :<|> driverActivity merchantId
    :<|> enableDrivers merchantId
    :<|> disableDrivers merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> unlinkVehicle merchantId
    :<|> updatePhoneNumber merchantId

listDriver :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver userMerchantId merchantId mbLimit mbOffset verified enabled phone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.listDrivers) mbLimit mbOffset verified enabled phone

driverActivity :: ShortId DM.Merchant -> ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity userMerchantId merchantId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.driverActivity)

enableDrivers :: ShortId DM.Merchant -> ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers userMerchantId merchantId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.enableDrivers) req

disableDrivers :: ShortId DM.Merchant -> ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers userMerchantId merchantId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.disableDrivers) req

driverLocation :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation userMerchantId merchantId mbLimit mbOffset req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.driverLocation) mbLimit mbOffset req

driverInfo :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo userMerchantId merchantId mbMobileNumber mbVehicleNumber = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  when (isJust mbMobileNumber == isJust mbVehicleNumber) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  Client.callBecknTransportBPP checkedMerchantId (.drivers.driverInfo) mbMobileNumber mbVehicleNumber

deleteDriver :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver userMerchantId merchantId driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.deleteDriver) driverId

unlinkVehicle :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
unlinkVehicle userMerchantId merchantId driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.unlinkVehicle) driverId

updatePhoneNumber :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> FlowHandler APISuccess
updatePhoneNumber userMerchantId merchantId driverId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callBecknTransportBPP checkedMerchantId (.drivers.updatePhoneNumber) driverId req
