{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Tickets where

import qualified Data.Text as Data.Text
import qualified Domain as Domain
import qualified Domain.Action.UI.TicketService as Domain.Action.UI.TicketService
import qualified Domain.Types.Merchant as Domain.Types.Merchant
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Environment as Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface as Kernel.External.Payment.Interface
import Kernel.Utils.Common
import qualified Kernel.Utils.Id as Kernel.Utils.Id
import Servant
import Tools.Auth

type API =
  TokenAuth :> "ticket" :> "places" :> "bookings" :> Capture "personServiceId" Kernel.Utils.Id.Id Domain.TicketService :> Capture "ticketServiceShortId" Kernel.Utils.Id.Id Domain.TicketBooking :> "details" :> QueryParam "limit" Kernel.Utils.Id.Id Domain.TicketBooking :> QueryParam "status" Kernel.Utils.Id.Id Domain.TicketService :> Header "bla" Data.Text.Text :> Header "bla2" Data.Text.Text :> ReqBody '[JSON] Domain.TicketBookingDetails :> Get '[JSON] Domain.TicketBookingDetails
    :<|> TokenAuth :> "ticket" :> "places" :> "book" :> ReqBody '[JSON] Domain.TicketBookingReq :> Post '[JSON] Kernel.External.Payment.Interface.CreateOrderResp

handler :: Environment.FlowServer API
handler =
  getTicketPlacesBookingsDetails
    :<|> postTicketPlacesBook

getTicketPlacesBookingsDetails :: (Id Domain.Types.Person.Person.Person, Id Domain.Types.Merchant.Merchant.Merchant) -> Kernel.Utils.Id.Id Domain.TicketService -> Kernel.Utils.Id.Id Domain.TicketBooking -> Kernel.Utils.Id.Id Domain.TicketBooking -> Kernel.Utils.Id.Id Domain.TicketService -> Data.Text.Text -> Data.Text.Text -> Domain.TicketBookingDetails -> Environment.FlowHandler Domain.TicketBookingDetails
getTicketPlacesBookingsDetails = withFlowHandlerAPI . Domain.Action.UI.TicketService.getTicketPlacesBookingsDetails

postTicketPlacesBook :: (Id Domain.Types.Person.Person.Person, Id Domain.Types.Merchant.Merchant.Merchant) -> Domain.TicketBookingReq -> Environment.FlowHandler Kernel.External.Payment.Interface.CreateOrderResp
postTicketPlacesBook = withFlowHandlerAPI . Domain.Action.UI.TicketService.postTicketPlacesBook
