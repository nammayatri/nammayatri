{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketService
  ( API,
    handler,
  )
where

import qualified API.Types.UI.TicketService
import qualified Control.Lens
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Action.UI.TicketService
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketPlace
import qualified Domain.Types.TicketService
import qualified Domain.Types.TicketSubPlace
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "ticket" :> "places" :> Get ('[JSON]) [Domain.Types.TicketPlace.TicketPlace] :<|> TokenAuth :> "ticket" :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "services"
      :> QueryParam "date" Data.Time.Calendar.Day
      :> QueryParam
           "subPlaceId"
           (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace)
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketServiceResp]
      :<|> TokenAuth
      :> "ticket"
      :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "book"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingReq
      :> Post
           ('[JSON])
           Kernel.External.Payment.Interface.Types.CreateOrderResp
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "status"
           Domain.Types.TicketBooking.BookingStatus
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketBookingAPIEntity]
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> "v2"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.TicketBooking.BookingStatus
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketBookingAPIEntityV2]
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> Capture
           "ticketBookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking)
      :> "details"
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingDetails
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> Capture
           "personServiceId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> Capture
           "ticketServiceShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> "verify"
      :> "v2"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationReq
      :> Post
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationResp
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> Capture
           "personServiceId"
           (Kernel.Types.Id.Id Domain.Types.TicketService.TicketService)
      :> Capture
           "ticketServiceShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService)
      :> "verify"
      :> QueryParam
           "fleetOwnerId"
           Data.Text.Text
      :> QueryParam
           "vehicleNo"
           Data.Text.Text
      :> Post
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceVerificationResp
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> Capture
           "ticketBookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking)
      :> "status"
      :> Get
           ('[JSON])
           Domain.Types.TicketBooking.BookingStatus
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> Capture
           "ticketBookingShortId"
           (Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking)
      :> "cashCollect"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "ticket"
      :> "booking"
      :> "cancel"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingCancelReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "ticket"
      :> "bookings"
      :> "update"
      :> "seats"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketBookingUpdateSeatsReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "ticket"
      :> "service"
      :> "cancel"
      :> ReqBody
           ('[JSON])
           API.Types.UI.TicketService.TicketServiceCancelReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "ticket"
      :> "place"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "availability"
      :> QueryParam
           "forceFresh"
           Kernel.Prelude.Bool
      :> QueryParam
           "isClosed"
           Kernel.Prelude.Bool
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketPlaceAvailability]
      :<|> TokenAuth
      :> "ticket"
      :> "places"
      :> "v2"
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketPlaceResp]
      :<|> TokenAuth
      :> "ticket"
      :> "place"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketPlaceResp
      :<|> TokenAuth
      :> "ticket"
      :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "fleet"
      :> "VehicleAssociation"
      :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "searchString"
           Data.Text.Text
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketFleetVehicleResp]
      :<|> TokenAuth
      :> "ticket"
      :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "fleet"
      :> "VehicleAssociation"
      :> "list"
      :> "v2"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "searchString"
           Data.Text.Text
      :> Get
           ('[JSON])
           [API.Types.UI.TicketService.TicketFleetVehicleResp]
      :<|> TokenAuth
      :> "ticket"
      :> "places"
      :> Capture
           "placeId"
           (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace)
      :> "bookings"
      :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "status"
           Domain.Types.TicketBooking.BookingStatus
      :> Get
           ('[JSON])
           API.Types.UI.TicketService.TicketPlaceBookingList
  )

handler :: Environment.FlowServer API
handler = getTicketPlaces :<|> getTicketPlacesServices :<|> postTicketPlacesBook :<|> getTicketBookings :<|> getTicketBookingsV2 :<|> getTicketBookingsDetails :<|> postTicketBookingsVerifyV2 :<|> postTicketBookingsVerify :<|> getTicketBookingsStatus :<|> postTicketBookingsCashCollect :<|> postTicketBookingCancel :<|> postTicketBookingsUpdateSeats :<|> postTicketServiceCancel :<|> getTicketPlaceAvailability :<|> getTicketPlacesV2 :<|> getTicketPlace :<|> getTicketFleetVehicles :<|> getTicketFleetVehiclesV2 :<|> getTicketPlaceBookings

getTicketPlaces :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketPlaces a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlaces (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getTicketPlacesServices ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Kernel.Prelude.Maybe (Data.Time.Calendar.Day) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace) ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp]
  )
getTicketPlacesServices a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlacesServices (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

postTicketPlacesBook ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    API.Types.UI.TicketService.TicketBookingReq ->
    Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp
  )
postTicketPlacesBook a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketPlacesBook (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getTicketBookings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Domain.Types.TicketBooking.BookingStatus ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketBookingAPIEntity]
  )
getTicketBookings a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getTicketBookingsV2 ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Domain.Types.TicketBooking.BookingStatus) ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketBookingAPIEntityV2]
  )
getTicketBookingsV2 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookingsV2 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getTicketBookingsDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
    Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails
  )
getTicketBookingsDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookingsDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postTicketBookingsVerifyV2 ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketService.TicketService ->
    Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService ->
    API.Types.UI.TicketService.TicketServiceVerificationReq ->
    Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp
  )
postTicketBookingsVerifyV2 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsVerifyV2 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

postTicketBookingsVerify ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketService.TicketService ->
    Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp
  )
postTicketBookingsVerify a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsVerify (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getTicketBookingsStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
    Environment.FlowHandler Domain.Types.TicketBooking.BookingStatus
  )
getTicketBookingsStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketBookingsStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postTicketBookingsCashCollect ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postTicketBookingsCashCollect a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsCashCollect (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postTicketBookingCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.TicketService.TicketBookingCancelReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postTicketBookingCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postTicketBookingsUpdateSeats ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.TicketService.TicketBookingUpdateSeatsReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postTicketBookingsUpdateSeats a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketBookingsUpdateSeats (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postTicketServiceCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.TicketService.TicketServiceCancelReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postTicketServiceCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.postTicketServiceCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getTicketPlaceAvailability ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketPlaceAvailability]
  )
getTicketPlaceAvailability a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlaceAvailability (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getTicketPlacesV2 :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.TicketService.TicketPlaceResp])
getTicketPlacesV2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlacesV2 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getTicketPlace ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Environment.FlowHandler API.Types.UI.TicketService.TicketPlaceResp
  )
getTicketPlace a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlace (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getTicketFleetVehicles ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp]
  )
getTicketFleetVehicles a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketFleetVehicles (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getTicketFleetVehiclesV2 ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Environment.FlowHandler [API.Types.UI.TicketService.TicketFleetVehicleResp]
  )
getTicketFleetVehiclesV2 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketFleetVehiclesV2 (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getTicketPlaceBookings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Domain.Types.TicketBooking.BookingStatus ->
    Environment.FlowHandler API.Types.UI.TicketService.TicketPlaceBookingList
  )
getTicketPlaceBookings a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketService.getTicketPlaceBookings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1
