module API.UI.Booking.BookingId.TriggerStatus.Handler where

import API.UI.Booking.BookingId.TriggerStatus.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.ACL.Status as BecknACL
import qualified Domain.Endpoints.UI.TriggerStatus as DStatus
import qualified Domain.Types.Booking.Type as DBooking (Booking)
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Auth

handler :: FlowServer API
handler = triggerStatusUpdate

triggerStatusUpdate :: PersonId -> Id DBooking.Booking -> FlowHandler APISuccess
triggerStatusUpdate _ bookingId = withFlowHandlerAPI $ do
  statusReq <- DStatus.triggerStatusUpdate bookingId
  becknStatusReq <- BecknACL.buildStatusReq statusReq
  ExternalAPI.status statusReq.bppUrl becknStatusReq
  pure Success
