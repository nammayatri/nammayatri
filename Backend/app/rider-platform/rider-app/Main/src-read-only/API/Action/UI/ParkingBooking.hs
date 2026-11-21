{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ParkingBooking
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ParkingBooking
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.ParkingBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBookingService
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( "multimodal" :> "parking" :> "book" :> Header "api-key" Data.Text.Text :> ReqBody '[JSON] API.Types.UI.ParkingBooking.ParkingBookingReq
      :> Post
           '[JSON]
           API.Types.UI.ParkingBooking.ParkingBookingResponse
      :<|> "multimodal"
      :> "parking"
      :> "marshal"
      :> "create"
      :> Header
           "api-key"
           Data.Text.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.ParkingBooking.MarshalPersonReq
      :> Post
           '[JSON]
           API.Types.UI.ParkingBooking.MarshalPersonResp
      :<|> TokenAuth
      :> "parking"
      :> "zones"
      :> QueryParam
           "durationMinutes"
           Kernel.Prelude.Int
      :> QueryParam
           "radiusMeters"
           Kernel.Prelude.Int
      :> QueryParam
           "startTime"
           Kernel.Prelude.UTCTime
      :> MandatoryQueryParam
           "lat"
           Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> Get
           '[JSON]
           [API.Types.UI.ParkingBooking.ParkingZoneResp]
      :<|> TokenAuth
      :> "parking"
      :> "active"
      :> Get
           '[JSON]
           [API.Types.UI.ParkingBooking.ParkingStayResp]
      :<|> TokenAuth
      :> "parking"
      :> "bookings"
      :> Get
           '[JSON]
           [API.Types.UI.ParkingBooking.ParkingStayResp]
      :<|> TokenAuth
      :> "parking"
      :> "bookings"
      :> Capture
           "bookingServiceId"
           (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService)
      :> "extend"
      :> ReqBody
           '[JSON]
           API.Types.UI.ParkingBooking.ParkingExtendReq
      :> Post
           '[JSON]
           Kernel.External.Payment.Interface.Types.CreateOrderResp
      :<|> TokenAuth
      :> "parking"
      :> "bookings"
      :> Capture
           "bookingServiceId"
           (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService)
      :> "stop"
      :> Post
           '[JSON]
           API.Types.UI.ParkingBooking.ParkingStopResp
  )

handler :: Environment.FlowServer API
handler = postMultimodalParkingBook :<|> postMultimodalParkingMarshalCreate :<|> getParkingZones :<|> getParkingActive :<|> getParkingBookings :<|> postParkingBookingsExtend :<|> postParkingBookingsStop

postMultimodalParkingBook :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.ParkingBooking.ParkingBookingReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingBookingResponse)
postMultimodalParkingBook a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.ParkingBooking.postMultimodalParkingBook a2 a1

postMultimodalParkingMarshalCreate :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.ParkingBooking.MarshalPersonReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.MarshalPersonResp)
postMultimodalParkingMarshalCreate a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.ParkingBooking.postMultimodalParkingMarshalCreate a2 a1

getParkingZones ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
    Kernel.Prelude.Double ->
    Kernel.Prelude.Double ->
    Environment.FlowHandler [API.Types.UI.ParkingBooking.ParkingZoneResp]
  )
getParkingZones a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a6) $ Domain.Action.UI.ParkingBooking.getParkingZones (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

getParkingActive :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.ParkingBooking.ParkingStayResp])
getParkingActive a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.ParkingBooking.getParkingActive (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getParkingBookings :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.ParkingBooking.ParkingStayResp])
getParkingBookings a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.ParkingBooking.getParkingBookings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postParkingBookingsExtend ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService ->
    API.Types.UI.ParkingBooking.ParkingExtendReq ->
    Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp
  )
postParkingBookingsExtend a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.ParkingBooking.postParkingBookingsExtend (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postParkingBookingsStop ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService ->
    Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingStopResp
  )
postParkingBookingsStop a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.ParkingBooking.postParkingBookingsStop (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
