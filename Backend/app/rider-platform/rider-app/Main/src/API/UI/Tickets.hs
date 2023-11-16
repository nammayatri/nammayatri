{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Tickets
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Tickets as DTB
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import qualified Domain.Types.Tickets as DTB
import qualified Domain.Types.Tickets.TicketBooking as DTB
import qualified Environment as App
import EulerHS.Prelude hiding (length)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

-------- Support Flow----------
type API =
  "ticket"
    :> ( "places"
           :> ( TokenAuth
                  :> Get '[JSON] [DTB.TicketPlace]
                  :<|> TokenAuth
                  :> Capture "placeId" (Id DTB.TicketPlace)
                  :> "services"
                  :> Get '[JSON] [DTB.TicketService]
                  :<|> TokenAuth
                  :> Capture "placeId" (Id DTB.TicketPlace)
                  :> "book"
                  :> ReqBody '[JSON] DTB.TicketBookingReq
                  :> Post '[JSON] Payment.CreateOrderResp
              )
           :<|> "bookings"
             :> ( TokenAuth
                    :> MandatoryQueryParam "status" DTB.BookingStatus
                    :> QueryParam "limit" Int
                    :> QueryParam "offset" Int
                    :> Get '[JSON] [DTB.TicketBookingAPIEntity]
                    :<|> TokenAuth
                    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBooking)
                    :> "details"
                    :> Get '[JSON] DTB.TicketBookingDetails
                    :<|> TokenAuth
                    :> Capture "personServiceId" (Id DTB.TicketService)
                    :> Capture "ticketServiceShortId" (ShortId DTB.TicketBookingService)
                    :> "verify"
                    :> Post '[JSON] DTB.TicketServiceVerificationResp
                    :<|> TokenAuth
                    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBooking)
                    :> "status"
                    :> Get '[JSON] DTB.BookingStatus
                )
       )

handler :: App.FlowServer API
handler =
  ( getTicketPlaces
      :<|> getTicketServices
      :<|> bookTicket
  )
    :<|> ( getAllBookings
             :<|> getBookingDetail
             :<|> verifyBookingDetails
             :<|> getBookingStatus
         )

getTicketPlaces :: (Id Person.Person, Id Merchant.Merchant) -> App.FlowHandler [DTB.TicketPlace]
getTicketPlaces (personId, merchantId) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DTB.getTicketPlaces (personId, merchantId)

getTicketServices :: (Id Person.Person, Id Merchant.Merchant) -> Id DTB.TicketPlace -> App.FlowHandler [DTB.TicketService]
getTicketServices (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.getTicketServices (personId, merchantId)

bookTicket :: (Id Person.Person, Id Merchant.Merchant) -> Id DTB.TicketPlace -> DTB.TicketBookingReq -> App.FlowHandler Payment.CreateOrderResp
bookTicket (personId, merchantId) placeId = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.bookTicket (personId, merchantId) placeId

getAllBookings :: (Id Person.Person, Id Merchant.Merchant) -> DTB.BookingStatus -> Maybe Int -> Maybe Int -> App.FlowHandler [DTB.TicketBookingAPIEntity]
getAllBookings (personId, merchantId) status mbLimit = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.getAllBookings (personId, merchantId) status mbLimit

getBookingDetail :: (Id Person.Person, Id Merchant.Merchant) -> ShortId DTB.TicketBooking -> App.FlowHandler DTB.TicketBookingDetails
getBookingDetail (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.getBookingDetail (personId, merchantId)

verifyBookingDetails :: (Id Person.Person, Id Merchant.Merchant) -> Id DTB.TicketService -> ShortId DTB.TicketBookingService -> App.FlowHandler DTB.TicketServiceVerificationResp
verifyBookingDetails (personId, _) personServiceId = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.verifyBookingDetails personServiceId

getBookingStatus :: (Id Person.Person, Id Merchant.Merchant) -> ShortId DTB.TicketBooking -> App.FlowHandler DTB.BookingStatus
getBookingStatus (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DTB.getBookingStatus (personId, merchantId)
