{-# LANGUAGE AllowAmbiguousTypes #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.BharatTaxi.User
  ( API,
    handler,
  )
where

import Data.Aeson (Value)
import qualified Domain.Types.Role as DRole
import "lib-dashboard" Domain.Types.ServerName as DSN
import "lib-dashboard" Environment
import qualified EulerHS.Types as ET
import Kernel.Prelude hiding (toList)
import Kernel.Utils.Common (withFlowHandlerAPI')
import Servant
import Tools.Auth.Dashboard (DashboardAuth, TokenInfo)
import "lib-dashboard" Tools.Client as Client

type ExternalFromListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "from-list"
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalToListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "to-list"
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalEstimateAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "estimate"
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value

type ExternalBookingAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "booking"
    :> QueryParam' '[Required] "estimateId" Text
    :> QueryParam' '[Required] "rider_id" Text
    :> QueryParam' '[Required] "payment_mode" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Post '[JSON] Value

type ExternalInvoiceAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "invoice"
    :> QueryParam' '[Required] "bookingId" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalBookingLatestAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "booking"
    :> "latest"
    :> QueryParam' '[Required] "riderId" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalBookingByIdAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "booking"
    :> Capture "bookingId" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalUpdateBookingAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "booking"
    :> Capture "bookingId" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] Value
    :> Put '[JSON] Value

type ExternalVehiclesListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "vehicles"
    :> QueryParam "driverNo" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalVehiclesCreateAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "vehicles"
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value

type ExternalDriversListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "drivers"
    :> QueryParam "vehicleNumber" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalDriversCreateAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "drivers"
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value

type API =
  "bharatTaxi"
    :> DashboardAuth 'DRole.DASHBOARD_USER
    :> ( "fromList"
           :> Get '[JSON] Value
           :<|> "toList"
           :> Get '[JSON] Value
           :<|> "estimate"
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
           :<|> "booking"
           :> QueryParam' '[Required] "estimateId" Text
           :> QueryParam' '[Required] "rider_id" Text
           :> QueryParam' '[Required] "payment_mode" Text
           :> Post '[JSON] Value
           :<|> "invoice"
           :> QueryParam' '[Required] "bookingId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> "latest"
           :> QueryParam' '[Required] "riderId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> Capture "bookingId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> Capture "bookingId" Text
           :> ReqBody '[JSON] Value
           :> Put '[JSON] Value
           :<|> "vehicles"
           :> QueryParam "driverNo" Text
           :> Get '[JSON] Value
           :<|> "vehicles"
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
           :<|> "drivers"
           :> QueryParam "vehicleNumber" Text
           :> Get '[JSON] Value
           :<|> "drivers"
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
       )

handler :: FlowServer API
handler tokenInfo =
  fromList tokenInfo
    :<|> toList tokenInfo
    :<|> estimate tokenInfo
    :<|> booking tokenInfo
    :<|> invoice tokenInfo
    :<|> bookingLatest tokenInfo
    :<|> bookingById tokenInfo
    :<|> updateBooking tokenInfo
    :<|> vehiclesList tokenInfo
    :<|> vehiclesCreate tokenInfo
    :<|> driversList tokenInfo
    :<|> driversCreate tokenInfo

externalFromListAPI :: Proxy ExternalFromListAPI
externalFromListAPI = Proxy

externalFromListClient :: Maybe Text -> Maybe Text -> ET.EulerClient Value
externalFromListClient = ET.client externalFromListAPI

externalToListAPI :: Proxy ExternalToListAPI
externalToListAPI = Proxy

externalToListClient :: Maybe Text -> Maybe Text -> ET.EulerClient Value
externalToListClient = ET.client externalToListAPI

externalEstimateAPI :: Proxy ExternalEstimateAPI
externalEstimateAPI = Proxy

externalEstimateClient :: Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalEstimateClient = ET.client externalEstimateAPI

externalBookingAPI :: Proxy ExternalBookingAPI
externalBookingAPI = Proxy

externalBookingClient :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalBookingClient = ET.client externalBookingAPI

externalInvoiceAPI :: Proxy ExternalInvoiceAPI
externalInvoiceAPI = Proxy

externalInvoiceClient :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalInvoiceClient = ET.client externalInvoiceAPI

externalBookingLatestAPI :: Proxy ExternalBookingLatestAPI
externalBookingLatestAPI = Proxy

externalBookingLatestClient :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalBookingLatestClient = ET.client externalBookingLatestAPI

externalBookingByIdAPI :: Proxy ExternalBookingByIdAPI
externalBookingByIdAPI = Proxy

externalBookingByIdClient :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalBookingByIdClient = ET.client externalBookingByIdAPI

externalUpdateBookingAPI :: Proxy ExternalUpdateBookingAPI
externalUpdateBookingAPI = Proxy

externalUpdateBookingClient :: Text -> Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalUpdateBookingClient = ET.client externalUpdateBookingAPI

externalVehiclesListAPI :: Proxy ExternalVehiclesListAPI
externalVehiclesListAPI = Proxy

externalVehiclesListClient :: Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalVehiclesListClient = ET.client externalVehiclesListAPI

externalVehiclesCreateAPI :: Proxy ExternalVehiclesCreateAPI
externalVehiclesCreateAPI = Proxy

externalVehiclesCreateClient :: Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalVehiclesCreateClient = ET.client externalVehiclesCreateAPI

externalDriversListAPI :: Proxy ExternalDriversListAPI
externalDriversListAPI = Proxy

externalDriversListClient :: Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalDriversListClient = ET.client externalDriversListAPI

externalDriversCreateAPI :: Proxy ExternalDriversCreateAPI
externalDriversCreateAPI = Proxy

externalDriversCreateClient :: Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalDriversCreateClient = ET.client externalDriversCreateAPI

-- | Bharat Taxi API DSL used with generic dashboard client.
data BharatTaxiAPIs = BharatTaxiAPIs
  { fromListDSL :: ET.EulerClient Value,
    toListDSL :: ET.EulerClient Value,
    estimateDSL :: Value -> ET.EulerClient Value,
    bookingDSL :: Text -> Text -> Text -> ET.EulerClient Value,
    invoiceDSL :: Text -> ET.EulerClient Value,
    bookingLatestDSL :: Text -> ET.EulerClient Value,
    bookingByIdDSL :: Text -> ET.EulerClient Value,
    updateBookingDSL :: Text -> Value -> ET.EulerClient Value,
    vehiclesListDSL :: Maybe Text -> ET.EulerClient Value,
    vehiclesCreateDSL :: Value -> ET.EulerClient Value,
    driversListDSL :: Maybe Text -> ET.EulerClient Value,
    driversCreateDSL :: Value -> ET.EulerClient Value
  }

-- | Build BharatTaxiAPIs from the configured data-server token.
mkBharatTaxiAPIs :: Text -> BharatTaxiAPIs
mkBharatTaxiAPIs token =
  let fromListDSL = externalFromListClient (Just "application/json") (Just token)
      toListDSL = externalToListClient (Just "application/json") (Just token)
      estimateDSL = externalEstimateClient (Just "application/json") (Just token)
      bookingDSL estimateId riderId paymentMode = externalBookingClient estimateId riderId paymentMode (Just "application/json") (Just token)
      invoiceDSL bookingId = externalInvoiceClient bookingId (Just "application/json") (Just token)
      bookingLatestDSL riderId = externalBookingLatestClient riderId (Just "application/json") (Just token)
      bookingByIdDSL bookingId = externalBookingByIdClient bookingId (Just "application/json") (Just token)
      updateBookingDSL bookingId = externalUpdateBookingClient bookingId (Just "application/json") (Just token)
      vehiclesListDSL driverNo = externalVehiclesListClient driverNo (Just "application/json") (Just token)
      vehiclesCreateDSL = externalVehiclesCreateClient (Just "application/json") (Just token)
      driversListDSL vehicleNumber = externalDriversListClient vehicleNumber (Just "application/json") (Just token)
      driversCreateDSL = externalDriversCreateClient (Just "application/json") (Just token)
   in BharatTaxiAPIs {..}

-- | Generic caller for Bharat Taxi service, similar to callFleetAPI.
callBharatTaxiAPI ::
  forall m r b c.
  Client.DashboardClient BharatTaxiAPIs m r b c =>
  ((BharatTaxiAPIs -> b) -> c)
callBharatTaxiAPI =
  Client.callServerAPI @_ @m @r DSN.BHARAT_TAXI mkBharatTaxiAPIs "callBharatTaxiAPI"

fromList :: TokenInfo -> FlowHandler Value
fromList _ = withFlowHandlerAPI' $ callBharatTaxiAPI (.fromListDSL)

toList :: TokenInfo -> FlowHandler Value
toList _ = withFlowHandlerAPI' $ callBharatTaxiAPI (.toListDSL)

estimate :: TokenInfo -> Value -> FlowHandler Value
estimate _ req = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.estimateDSL req)

booking :: TokenInfo -> Text -> Text -> Text -> FlowHandler Value
booking _ estimateId riderId paymentMode = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingDSL estimateId riderId paymentMode)

invoice :: TokenInfo -> Text -> FlowHandler Value
invoice _ bookingId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.invoiceDSL bookingId)

bookingLatest :: TokenInfo -> Text -> FlowHandler Value
bookingLatest _ riderId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingLatestDSL riderId)

bookingById :: TokenInfo -> Text -> FlowHandler Value
bookingById _ bookingId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingByIdDSL bookingId)

updateBooking :: TokenInfo -> Text -> Value -> FlowHandler Value
updateBooking _ bookingId body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.updateBookingDSL bookingId body)

vehiclesList :: TokenInfo -> Maybe Text -> FlowHandler Value
vehiclesList _ driverNo = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.vehiclesListDSL driverNo)

vehiclesCreate :: TokenInfo -> Value -> FlowHandler Value
vehiclesCreate _ body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.vehiclesCreateDSL body)

driversList :: TokenInfo -> Maybe Text -> FlowHandler Value
driversList _ vehicleNumber = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.driversListDSL vehicleNumber)

driversCreate :: TokenInfo -> Value -> FlowHandler Value
driversCreate _ body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.driversCreateDSL body)
