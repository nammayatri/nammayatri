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
import qualified Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Servant hiding (throwError)
import "lib-dashboard" Tools.Auth

type API =
  "driver"
    :> ( DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> DeleteDriverAPI
       )

type DriverListAPI =
  "list"
    :> ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverListAPI

type DriverActivityAPI =
  "activity"
    :> ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverActivityAPI

type EnableDriversAPI =
  "enable"
    :> ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.EnableDriversAPI

type DisableDriversAPI =
  "disable"
    :> ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.DisableDriversAPI

type DriverLocationAPI =
  "location"
    :> ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverLocationAPI

type DriverInfoAPI =
  "info"
    :> ApiAuth 'BECKN_TRANSPORT 'READ_ACCESS 'DRIVERS
    :> Common.DriverInfoAPI

type DeleteDriverAPI =
  ApiAuth 'BECKN_TRANSPORT 'WRITE_ACCESS 'DRIVERS
    :> Common.DeleteDriverAPI

handler :: FlowServer API
handler =
  listDriver
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation
    :<|> driverInfo
    :<|> deleteDriver

listDriver :: ShortId DMerchant.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver _ mbLimit mbOffset verified rejected pendingdoc phone = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.listDrivers) mbLimit mbOffset verified rejected pendingdoc phone

driverActivity :: ShortId DMerchant.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.driverActivity)

enableDrivers :: ShortId DMerchant.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.enableDrivers) req

disableDrivers :: ShortId DMerchant.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.disableDrivers) req

driverLocation :: ShortId DMerchant.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ mbLimit mbOffset req = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.driverLocation) mbLimit mbOffset req

driverInfo :: ShortId DMerchant.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo _ mbMobileNumber mbVehicleNumber = withFlowHandlerAPI $ do
  when (isJust mbMobileNumber == isJust mbVehicleNumber) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  Client.callBecknTransportBPP (.drivers.driverInfo) mbMobileNumber mbVehicleNumber

deleteDriver :: ShortId DMerchant.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver _ driverId = withFlowHandlerAPI $ do
  Client.callBecknTransportBPP (.drivers.deleteDriver) driverId
