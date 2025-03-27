{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.MeterRide (getMeterRidePrice) where

import qualified API.Types.UI.PriceBreakup
import Data.OpenApi (ToSchema)
import qualified Domain.Action.UI.FareCalculator as FC
import Domain.Types
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude as Prelude
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.LocationUpdates.Internal as LU
import Servant hiding (throwError)
import SharedLogic.FarePolicy
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Auth

getMeterRidePrice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes)
getMeterRidePrice _merchantShortId _opCity rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let tripStartTime = ride.tripStartTime
  let rideStatus = ride.status
  let driverId = ride.driverId
  merchantId <- fromMaybeM (RideNotFound rideId.getId) ride.merchantId
  let merchantOpCityId = ride.merchantOperatingCityId
  traveledDistance <- LU.getTravelledDistance driverId
  fareEstimates <- FC.calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters traveledDistance) Nothing Nothing (OneWay MeterRide) booking.configInExperimentVersions
  let mbMeterRideEstimate = Kernel.Prelude.listToMaybe fareEstimates.estimatedFares
  maybe
    (throwError . InternalError $ "Nahi aa rha hai fare :(" <> rideId.getId)
    ( \meterRideEstimate -> do
        return $ API.Types.UI.PriceBreakup.MeterRidePriceRes {fare = meterRideEstimate.minFare, distance = traveledDistance, tripStartTime = tripStartTime, status = Just rideStatus}
    )
    mbMeterRideEstimate
