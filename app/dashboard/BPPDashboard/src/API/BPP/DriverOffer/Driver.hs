module API.BPP.DriverOffer.Driver
  ( API,
    handler,
  )
where

import qualified API.BPP.DriverOffer.Driver.Registration as Reg
import qualified BPPClient.DriverOffer as Client
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "driver"
    :> ( DriverDocumentsInfoAPI
           :<|> DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
           :<|> Reg.API
       )

type DriverDocumentsInfoAPI =
  "documents"
    :> "info"
    :> ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverDocumentsInfoAPI

type DriverListAPI =
  "list"
    :> ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverListAPI

type DriverActivityAPI =
  "activity"
    :> ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverActivityAPI

type EnableDriversAPI =
  "enable"
    :> ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS
    :> Common.EnableDriversAPI

type DisableDriversAPI =
  "disable"
    :> ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'DRIVERS
    :> Common.DisableDriversAPI

type DriverLocationAPI =
  "location"
    :> ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'DRIVERS
    :> Common.DriverLocationAPI

handler :: FlowServer API
handler =
  driverDocuments
    :<|> listDriver
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation
    :<|> Reg.handler

driverDocuments :: ShortId DMerchant.Merchant -> FlowHandler Common.DriverDocumentsInfoRes
driverDocuments _ = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.driverDocumentsInfo)

listDriver :: ShortId DMerchant.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDriver _ mbLimit mbOffset verified rejected pendingdoc phone = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.listDrivers) mbLimit mbOffset verified rejected pendingdoc phone

driverActivity :: ShortId DMerchant.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.driverActivity)

enableDrivers :: ShortId DMerchant.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.enableDrivers) req

disableDrivers :: ShortId DMerchant.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.disableDrivers) req

driverLocation :: ShortId DMerchant.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ mbLimit mbOffset req = withFlowHandlerAPI $ do
  Client.callDriverOfferBPP (.drivers.driverLocation) mbLimit mbOffset req
