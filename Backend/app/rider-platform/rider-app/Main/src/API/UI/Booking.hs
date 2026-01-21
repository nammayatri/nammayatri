{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Booking
  ( DBooking.BookingListRes,
    API,
    handler,
    bookingStatus,
    bookingList,
  )
where

import qualified Domain.Action.UI.Booking as DBooking
import qualified Domain.Action.UI.InvoiceGeneration as DInvoice
import qualified Domain.Types.Booking as SRB
import Domain.Types.Booking.API (BookingAPIEntity, BookingRequestType, BookingStatusAPIEntity)
import qualified Domain.Types.BookingStatus as SRB
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.PassType
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Type as SLT
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "rideBooking"
    :> ( Capture "rideBookingId" (Id SRB.Booking)
           :> TokenAuth
           :> Post '[JSON] BookingAPIEntity
           :<|> "v2"
             :> Capture "rideBookingId" (Id SRB.Booking)
             :> TokenAuth
             :> Get '[JSON] BookingStatusAPIEntity
           :<|> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> QueryParam "status" SRB.BookingStatus
             :> QueryParam "clientId" (Id DC.Client)
             :> QueryParam "fromDate" Integer
             :> QueryParam "toDate" Integer
             :> QueryParams "rideStatus" SRB.BookingStatus
             :> Get '[JSON] DBooking.BookingListRes
           :<|> "listV2"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "bookingOffset" Integer
             :> QueryParam "journeyOffset" Integer
             :> QueryParam "passOffset" Integer
             :> QueryParam "fromDate" Integer
             :> QueryParam "toDate" Integer
             :> QueryParams "billingCategory" SLT.BillingCategory
             :> QueryParams "rideType" SLT.RideType
             :> QueryParams "rideStatus" SRB.BookingStatus
             :> QueryParams "journeyStatus" DJ.JourneyStatus
             :> QueryParam "isPaymentSuccess" Bool
             :> QueryParam "bookingRequestType" BookingRequestType
             :> QueryParam "sendEligiblePassIfAvailable" Bool
             :> QueryParams "passTypes" Domain.Types.PassType.PassEnum
             :> Get '[JSON] DBooking.BookingListResV2
           :<|> "favourites"
             :> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> QueryParam "status" SRB.BookingStatus
             :> QueryParam "clientId" (Id DC.Client)
             :> ReqBody '[JSON] DBooking.DriverNo
             :> Post '[JSON] DBooking.FavouriteBookingListRes
           :<|> Capture "rideBookingId" (Id SRB.Booking)
             :> TokenAuth
             :> "addStop"
             :> ReqBody '[JSON] DBooking.StopReq
             :> Post '[JSON] APISuccess
           :<|> Capture "rideBookingId" (Id SRB.Booking)
             :> TokenAuth
             :> "editStop"
             :> ReqBody '[JSON] DBooking.StopReq
             :> Post '[JSON] APISuccess
           :<|> "invoice"
             :> "generate"
             :> TokenAuth
             :> ReqBody '[JSON] DInvoice.GenerateInvoiceReq
             :> Post '[JSON] DInvoice.GenerateInvoiceRes
       )

handler :: FlowServer API
handler =
  bookingStatus
    :<|> bookingStatusPolling
    :<|> bookingList
    :<|> bookingListV2
    :<|> favouriteBookingList
    :<|> addStop
    :<|> editStop
    :<|> generateInvoice

bookingStatus :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler BookingAPIEntity
bookingStatus bookingId = withFlowHandlerAPI . DBooking.bookingStatus bookingId

bookingStatusPolling :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler BookingStatusAPIEntity
bookingStatusPolling bookingId = withFlowHandlerAPI . DBooking.bookingStatusPolling bookingId

addStop :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> DBooking.StopReq -> FlowHandler APISuccess
addStop bookingId (personId, merchantId) addStopReq = withFlowHandlerAPI . withPersonIdLogTag personId $ DBooking.addStop (personId, merchantId) bookingId addStopReq

editStop :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> DBooking.StopReq -> FlowHandler APISuccess
editStop bookingId (personId, merchantId) editStopReq = withFlowHandlerAPI . withPersonIdLogTag personId $ DBooking.editStop (personId, merchantId) bookingId editStopReq

bookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> Maybe Integer -> Maybe Integer -> [SRB.BookingStatus] -> FlowHandler DBooking.BookingListRes
bookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive mbClientId mbFromDate mbToDate mbBookingStatusList = withFlowHandlerAPI . DBooking.bookingList (Just personId, merchantId) Nothing False mbLimit mbOffset mbOnlyActive mbClientId mbFromDate mbToDate mbBookingStatusList

bookingListV2 :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [SLT.BillingCategory] -> [SLT.RideType] -> [SRB.BookingStatus] -> [DJ.JourneyStatus] -> Maybe Bool -> Maybe BookingRequestType -> Maybe Bool -> [Domain.Types.PassType.PassEnum] -> FlowHandler DBooking.BookingListResV2
bookingListV2 (personId, merchantId) mbLimit mbOffset mbBookingOffset mbJourneyOffset mbPassOffset mbFromDate mbToDate billingCategoryList rideTypeList bookingStatusList journeyStatusList mbIsPaymentSuccess mbBookingRequestType mbSendEligiblePassIfAvailable passTypes = withFlowHandlerAPI $ DBooking.bookingListV2 (personId, merchantId) mbLimit mbOffset mbBookingOffset mbJourneyOffset mbPassOffset mbFromDate mbToDate billingCategoryList rideTypeList bookingStatusList journeyStatusList mbIsPaymentSuccess mbBookingRequestType mbSendEligiblePassIfAvailable (Just passTypes)

favouriteBookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> DBooking.DriverNo -> FlowHandler DBooking.FavouriteBookingListRes
favouriteBookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive mbClientId driver = withFlowHandlerAPI . DBooking.favouriteBookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive mbClientId driver

generateInvoice :: (Id Person.Person, Id Merchant.Merchant) -> DInvoice.GenerateInvoiceReq -> FlowHandler DInvoice.GenerateInvoiceRes
generateInvoice (personId, merchantId) req = withFlowHandlerAPI . withPersonIdLogTag personId $ DInvoice.generateInvoice (personId, merchantId) req
