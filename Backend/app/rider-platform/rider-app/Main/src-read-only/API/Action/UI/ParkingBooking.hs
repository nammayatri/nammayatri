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
      :> "ticket"
      :> "booking"
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
      :> "ticket"
      :> "booking"
      :> Capture
           "bookingServiceId"
           (Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService)
      :> "stop"
      :> Post
           '[JSON]
           API.Types.UI.ParkingBooking.ParkingStopResp
  )

handler :: Environment.FlowServer API
handler = postMultimodalParkingBook :<|> postMultimodalParkingMarshalCreate :<|> postTicketBookingExtend :<|> postTicketBookingStop

postMultimodalParkingBook :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.ParkingBooking.ParkingBookingReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingBookingResponse)
postMultimodalParkingBook a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.ParkingBooking.postMultimodalParkingBook a2 a1

postMultimodalParkingMarshalCreate :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.ParkingBooking.MarshalPersonReq -> Environment.FlowHandler API.Types.UI.ParkingBooking.MarshalPersonResp)
postMultimodalParkingMarshalCreate a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.ParkingBooking.postMultimodalParkingMarshalCreate a2 a1

postTicketBookingExtend ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService ->
    API.Types.UI.ParkingBooking.ParkingExtendReq ->
    Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateOrderResp
  )
postTicketBookingExtend a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.ParkingBooking.postTicketBookingExtend (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postTicketBookingStop ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService ->
    Environment.FlowHandler API.Types.UI.ParkingBooking.ParkingStopResp
  )
postTicketBookingStop a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.ParkingBooking.postTicketBookingStop (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
