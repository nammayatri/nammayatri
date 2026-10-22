module Domain.Action.UI.AutoAcceptRide (postDriverAutoAcceptRideAcknowledge) where

import qualified API.Types.UI.AutoAcceptRide as APIT
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingExtra as QBookingExtra
import Tools.Error

postDriverAutoAcceptRideAcknowledge ::
  ( ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id DRB.Booking ->
    APIT.AutoAcceptRideReq ->
    Environment.Flow APISuccess.APISuccess
  )
postDriverAutoAcceptRideAcknowledge (mbPersonId, _, _) bookingId req = do
  _personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  let ctaData =
        Just
          DRB.AutoAcceptCtaData
            { overlaySentAt = booking.autoAcceptCtaData >>= (.overlaySentAt),
              acknowledged = Just req.acknowledged
            }
  QBookingExtra.updateAutoAcceptCtaData bookingId ctaData
  pure APISuccess.Success
