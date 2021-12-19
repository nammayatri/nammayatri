module API.Parking.Booking.BookingId.TriggerStatus.Handler where

import API.Parking.Booking.BookingId.TriggerStatus.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Core.Migration.API.Types
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.Migration.Domain
import Beckn.Types.Id
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
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  bppOrderId <- booking.bppOrderId & fromMaybeM BookingBppOrderIdNotFound
  let url = booking.bppUrl
  context <- buildParkingContext STATUS (getId bookingId)
  ExternalAPI.triggerStatusUpdate url (BecknReq context (makeStatusMessage bppOrderId))
  pure Success
  where
    buildParkingContext action txnId = do
      currTime <- getCurrentTime
      msgId <- generateGUIDText
      bapId <- asks (.config.selfId)
      bapUri <- asks (.config.selfURI)
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
            transaction_id = txnId,
            message_id = msgId,
            timestamp = currTime,
            key = Nothing,
            ttl = Nothing
          }
    makeStatusMessage bppOrderId =
      Status.StatusMessage
        { order =
            Status.Order
              { id = bppOrderId
              }
        }
