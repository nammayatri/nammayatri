{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Tickets where

import qualified "this" API.Types.UI.TicketService
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = (PostTicketsVerify :<|> PostTicketsServices :<|> GetTicketsPlaces :<|> PostTicketsUpdate :<|> PostTicketsBookingsCancel :<|> PostTicketsServiceCancel :<|> GetTicketsBookingDetails)

type PostTicketsVerify =
  ( "tickets" :> Capture "personServiceId" (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> Capture
           "ticketBookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> "verify"
      :> Post
           '[JSON]
           API.Types.UI.TicketService.TicketServiceVerificationResp
  )

type PostTicketsServices =
  ( Capture "ticketPlaceId" (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace) :> "services" :> QueryParam "date" Data.Time.Calendar.Day
      :> Post
           '[JSON]
           [API.Types.UI.TicketService.TicketServiceResp]
  )

type GetTicketsPlaces = ("places" :> Get '[JSON] [Domain.Types.TicketPlace.TicketPlace])

type PostTicketsUpdate = ("update" :> ReqBody '[JSON] API.Types.UI.TicketService.TicketBookingUpdateSeatsReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostTicketsBookingsCancel = ("bookings" :> "cancel" :> ReqBody '[JSON] API.Types.UI.TicketService.TicketBookingCancelReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostTicketsServiceCancel = ("service" :> "cancel" :> ReqBody '[JSON] API.Types.UI.TicketService.TicketServiceCancelReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetTicketsBookingDetails =
  ( "booking" :> Capture "ticketBookingShortId" (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking) :> "details"
      :> Get
           '[JSON]
           API.Types.UI.TicketService.TicketBookingDetails
  )

data TicketsAPIs = TicketsAPIs
  { postTicketsVerify :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketServiceVerificationResp,
    postTicketsServices :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> EulerHS.Types.EulerClient [API.Types.UI.TicketService.TicketServiceResp],
    getTicketsPlaces :: EulerHS.Types.EulerClient [Domain.Types.TicketPlace.TicketPlace],
    postTicketsUpdate :: API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postTicketsBookingsCancel :: API.Types.UI.TicketService.TicketBookingCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postTicketsServiceCancel :: API.Types.UI.TicketService.TicketServiceCancelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getTicketsBookingDetails :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> EulerHS.Types.EulerClient API.Types.UI.TicketService.TicketBookingDetails
  }

mkTicketsAPIs :: (Client EulerHS.Types.EulerClient API -> TicketsAPIs)
mkTicketsAPIs ticketsClient = (TicketsAPIs {..})
  where
    postTicketsVerify :<|> postTicketsServices :<|> getTicketsPlaces :<|> postTicketsUpdate :<|> postTicketsBookingsCancel :<|> postTicketsServiceCancel :<|> getTicketsBookingDetails = ticketsClient

data TicketsUserActionType
  = POST_TICKETS_VERIFY
  | POST_TICKETS_SERVICES
  | GET_TICKETS_PLACES
  | POST_TICKETS_UPDATE
  | POST_TICKETS_BOOKINGS_CANCEL
  | POST_TICKETS_SERVICE_CANCEL
  | GET_TICKETS_BOOKING_DETAILS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''TicketsUserActionType])
