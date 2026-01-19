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

import qualified Dashboard.Common as Common
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Domain.Action.RiderPlatform.Management.Customer as Customer
import qualified "lib-dashboard" Domain.Types.AccessMatrix as DMatrix
import "lib-dashboard" Domain.Types.ServerName as DSN
import "lib-dashboard" Environment
import qualified EulerHS.Types as ET
import Kernel.Prelude hiding (toList)
import Kernel.Types.Error (PersonError (PersonDoesNotExist, PersonFieldNotPresent))
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, withFlowHandlerAPI')
import Servant
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth.Api (ApiAuth, ApiTokenInfo)
import "lib-dashboard" Tools.Client as Client

type ExternalFromListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "from-list"
    :> QueryParam "to-location" Text
    :> Header "accept" Text
    :> Header "token" Text
    :> Get '[JSON] Value

type ExternalToListAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "to-list"
    :> QueryParam "from-location" Text
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
    :> Header "accept" Text
    :> Header "token" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value

type ExternalInvoiceAPI =
  "delhi-temp"
    :> "api"
    :> "v1"
    :> "invoice"
    :> QueryParam' '[Required] "bookingId" Text
    :> QueryParam' '[Required] "riderPhoneNumber" Text
    :> QueryParam "riderName" Text
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
    :> ( "fromList"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_FROM_LIST
           :> QueryParam "to-location" Text
           :> Get '[JSON] Value
           :<|> "toList"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_TO_LIST
           :> QueryParam "from-location" Text
           :> Get '[JSON] Value
           :<|> "estimate"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_ESTIMATE
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
           :<|> "booking"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_BOOKING
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
           :<|> "invoice"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_INVOICE
           :> QueryParam' '[Required] "bookingId" Text
           :> QueryParam' '[Required] "riderId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> "latest"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_BOOKING_LATEST
           :> QueryParam' '[Required] "riderId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_BOOKING_BY_ID
           :> Capture "bookingId" Text
           :> Get '[JSON] Value
           :<|> "booking"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_UPDATE_BOOKING
           :> Capture "bookingId" Text
           :> ReqBody '[JSON] Value
           :> Put '[JSON] Value
           :<|> "vehicles"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_VEHICLES_LIST
           :> QueryParam "driverNo" Text
           :> Get '[JSON] Value
           :<|> "vehicles"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_VEHICLES_CREATE
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
           :<|> "drivers"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_DRIVERS_LIST
           :> QueryParam "vehicleNumber" Text
           :> Get '[JSON] Value
           :<|> "drivers"
           :> ApiAuth 'DSN.BHARAT_TAXI 'DMatrix.BHARAT_TAXI_USER 'DMatrix.BHARAT_TAXI_DRIVERS_CREATE
           :> ReqBody '[JSON] Value
           :> Post '[JSON] Value
       )

handler :: FlowServer API
handler =
  fromList
    :<|> toList
    :<|> estimate
    :<|> booking
    :<|> invoice
    :<|> bookingLatest
    :<|> bookingById
    :<|> updateBooking
    :<|> vehiclesList
    :<|> vehiclesCreate
    :<|> driversList
    :<|> driversCreate

externalFromListAPI :: Proxy ExternalFromListAPI
externalFromListAPI = Proxy

externalFromListClient :: Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalFromListClient = ET.client externalFromListAPI

externalToListAPI :: Proxy ExternalToListAPI
externalToListAPI = Proxy

externalToListClient :: Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
externalToListClient = ET.client externalToListAPI

externalEstimateAPI :: Proxy ExternalEstimateAPI
externalEstimateAPI = Proxy

externalEstimateClient :: Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalEstimateClient = ET.client externalEstimateAPI

externalBookingAPI :: Proxy ExternalBookingAPI
externalBookingAPI = Proxy

externalBookingClient :: Maybe Text -> Maybe Text -> Value -> ET.EulerClient Value
externalBookingClient = ET.client externalBookingAPI

externalInvoiceAPI :: Proxy ExternalInvoiceAPI
externalInvoiceAPI = Proxy

externalInvoiceClient :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> ET.EulerClient Value
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
  { fromListDSL :: Maybe Text -> ET.EulerClient Value,
    toListDSL :: Maybe Text -> ET.EulerClient Value,
    estimateDSL :: Value -> ET.EulerClient Value,
    bookingDSL :: Value -> ET.EulerClient Value,
    invoiceDSL :: Text -> Text -> Maybe Text -> ET.EulerClient Value,
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
  let fromListDSL toLocation = externalFromListClient toLocation (Just "application/json") (Just token)
      toListDSL fromLocation = externalToListClient fromLocation (Just "application/json") (Just token)
      estimateDSL = externalEstimateClient (Just "application/json") (Just token)
      bookingDSL = externalBookingClient (Just "application/json") (Just token)
      invoiceDSL bookingId riderPhoneNumber riderName = externalInvoiceClient bookingId riderPhoneNumber riderName (Just "application/json") (Just token)
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

fromList :: ApiTokenInfo -> Maybe Text -> FlowHandler Value
fromList _ toLocation = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.fromListDSL toLocation)

toList :: ApiTokenInfo -> Maybe Text -> FlowHandler Value
toList _ fromLocation = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.toListDSL fromLocation)

estimate :: ApiTokenInfo -> Value -> FlowHandler Value
estimate _ req = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.estimateDSL req)

booking :: ApiTokenInfo -> Value -> FlowHandler Value
booking _ req = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingDSL req)

invoice :: ApiTokenInfo -> Text -> Text -> FlowHandler Value
invoice apiTokenInfo bookingId riderId =
  withFlowHandlerAPI' $ do
    -- Extract rider phone number and name from rider-app person table using riderId
    let personId = Id riderId
        customerId = cast @Common.Person @Common.Customer personId
    -- Get customer information from rider-app using getCustomerList
    customerListRes <- Customer.getCustomerList apiTokenInfo.merchant.shortId apiTokenInfo.city apiTokenInfo Nothing Nothing Nothing Nothing Nothing (Just customerId)
    -- Extract the first customer from the list
    customer <- fromMaybeM (PersonDoesNotExist riderId) (listToMaybe customerListRes.customers)
    -- Extract phone number and name from customer
    riderPhoneNumber <- fromMaybeM (PersonFieldNotPresent $ "phoneNo (riderId: " <> riderId <> ")") customer.phoneNo
    let nameParts = filter (not . T.null) (catMaybes [customer.firstName, customer.middleName, customer.lastName])
        fullName = T.unwords nameParts
        riderName = if null nameParts then Nothing else Just fullName
    callBharatTaxiAPI (\apis -> apis.invoiceDSL bookingId riderPhoneNumber riderName)

bookingLatest :: ApiTokenInfo -> Text -> FlowHandler Value
bookingLatest _ riderId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingLatestDSL riderId)

bookingById :: ApiTokenInfo -> Text -> FlowHandler Value
bookingById _ bookingId = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.bookingByIdDSL bookingId)

updateBooking :: ApiTokenInfo -> Text -> Value -> FlowHandler Value
updateBooking _ bookingId body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.updateBookingDSL bookingId body)

vehiclesList :: ApiTokenInfo -> Maybe Text -> FlowHandler Value
vehiclesList _ driverNo = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.vehiclesListDSL driverNo)

vehiclesCreate :: ApiTokenInfo -> Value -> FlowHandler Value
vehiclesCreate _ body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.vehiclesCreateDSL body)

driversList :: ApiTokenInfo -> Maybe Text -> FlowHandler Value
driversList _ vehicleNumber = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.driversListDSL vehicleNumber)

driversCreate :: ApiTokenInfo -> Value -> FlowHandler Value
driversCreate _ body = withFlowHandlerAPI' $ callBharatTaxiAPI (\apis -> apis.driversCreateDSL body)
