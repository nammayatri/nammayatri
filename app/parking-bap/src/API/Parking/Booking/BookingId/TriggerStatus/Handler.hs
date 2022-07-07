module API.Parking.Booking.BookingId.TriggerStatus.Handler where

import API.Parking.Booking.BookingId.TriggerStatus.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Core.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Types.TimeRFC339
import Beckn.Utils.Common
import qualified Core.Status as Status
import qualified Domain.Booking.Type as DBooking (Booking)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Booking as QBooking
import Tools.Auth
import Tools.Error

handler :: FlowServer API
handler = triggerStatusUpdate

triggerStatusUpdate :: PersonId -> Id DBooking.Booking -> FlowHandler APISuccess
triggerStatusUpdate _ bookingId = withFlowHandlerAPI $ do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  bppOrderId <- booking.bppOrderId & fromMaybeM BookingBppOrderIdNotFound
  let url = booking.bppUrl
  context <- buildParkingContext STATUS (getId bookingId)
  ExternalAPI.triggerStatusUpdate url (BecknReq context (makeStatusMessage bppOrderId))
  pure Success
  where
    buildParkingContext action txnId = do
      currTime <- UTCTimeRFC3339 <$> getCurrentTime
      msgId <- generateGUIDText
      bapId <- asks (.selfId)
      bapUri <- asks (.selfURI)
      return $
        Context
          { domain = PARKING,
            country = "IND",
            city = "Kochi",
            action = action,
            core_version = "0.9.0",
            bap_id = bapId,
            bap_uri = bapUri,
            bpp_id = Nothing,
            bpp_uri = Nothing,
            transaction_id = Just txnId,
            message_id = msgId,
            timestamp = currTime
          }
    makeStatusMessage bppOrderId =
      Status.StatusMessage
        { order =
            Status.Order
              { id = bppOrderId
              }
        }
