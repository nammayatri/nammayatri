module API.BPP.DriverOffer.Driver
  ( API,
    handler,
  )
where

import qualified API.BPP.DriverOffer.Driver.Registration as Reg
import qualified BPPClient.DriverOffer as Client
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
    :> ( DriverDocumentsInfoAPI
           :<|> DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> DeleteDriverAPI
           :<|> Reg.API
       )

type DriverDocumentsInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverDocumentsInfoAPI

type DriverListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverListAPI

type DriverActivityAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverActivityAPI

type EnableDriversAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS
    :> Common.EnableDriversAPI

type DisableDriversAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS
    :> Common.DisableDriversAPI

type DriverLocationAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverLocationAPI

type DriverInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverInfoAPI

type DeleteDriverAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS
    :> Common.DeleteDriverAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  driverDocuments merchantId
    :<|> listDriver merchantId
    :<|> driverActivity merchantId
    :<|> enableDrivers merchantId
    :<|> disableDrivers merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> Reg.handler merchantId

driverDocuments :: ShortId DM.Merchant -> ShortId DM.Merchant -> FlowHandler Common.DriverDocumentsInfoRes
driverDocuments userMerchantId merchantId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverDocumentsInfo)

listDriver :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver userMerchantId merchantId mbLimit mbOffset verified enabled phone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.listDrivers) mbLimit mbOffset verified enabled phone

driverActivity :: ShortId DM.Merchant -> ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity userMerchantId merchantId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverActivity)

enableDrivers :: ShortId DM.Merchant -> ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers userMerchantId merchantId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.enableDrivers) req

disableDrivers :: ShortId DM.Merchant -> ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers userMerchantId merchantId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.disableDrivers) req

driverLocation :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation userMerchantId merchantId mbLimit mbOffset req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverLocation) mbLimit mbOffset req

driverInfo :: ShortId DM.Merchant -> ShortId DM.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo userMerchantId merchantId mbMobileNumber mbVehicleNumber = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  when (isJust mbMobileNumber == isJust mbVehicleNumber) $
    throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  Client.callDriverOfferBPP checkedMerchantId (.drivers.driverInfo) mbMobileNumber mbVehicleNumber

deleteDriver :: ShortId DM.Merchant -> ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver userMerchantId merchantId driverId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck userMerchantId merchantId
  Client.callDriverOfferBPP checkedMerchantId (.drivers.deleteDriver) driverId
