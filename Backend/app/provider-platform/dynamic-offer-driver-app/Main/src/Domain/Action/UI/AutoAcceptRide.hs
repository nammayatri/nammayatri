module Domain.Action.UI.AutoAcceptRide (postDriverAutoAcceptRideAcknowledge) where

import qualified API.Types.UI.AutoAcceptRide as APIT
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

postDriverAutoAcceptRideAcknowledge ::
  ( ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
    APIT.AutoAcceptRideReq ->
    Environment.Flow APISuccess.APISuccess
  )
postDriverAutoAcceptRideAcknowledge (mbPersonId, _, _) bookingId req = do
  _personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _ <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  QBooking.updateAutoAcceptAcknowledged (Just req.acknowledged) bookingId
  pure APISuccess.Success
