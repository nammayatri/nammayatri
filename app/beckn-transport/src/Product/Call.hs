module Product.Call where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Flow (initiateCall)
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Taxi.API.Call
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as SRide
import Utils.Common

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM RideDoesNotExist
  rideBooking <-
    QRB.findById ride.bookingId
      >>= fromMaybeM RideBookingNotFound
  requestorPhone <- decrypt rideBooking.requestorMobileNumber
  driverPhone <- getDriverPhone ride
  initiateCall driverPhone requestorPhone
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from provider to customer."
  return Ack

getDriverPhone :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- QPerson.findPersonById driverId >>= fromMaybeM PersonNotFound
  decMobNum <- decrypt driver.mobileNumber
  let phonenum = (<>) <$> driver.mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")
