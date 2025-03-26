{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.MeterRide (getMeterRidePrice) where

import qualified API.Types.UI.PriceBreakup
import qualified Domain.Action.UI.FareCalculator as FC
import Domain.Types
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time as KTP
import Kernel.Utils.Common
import qualified Lib.LocationUpdates.Internal as LU
import qualified Storage.Queries.Ride as QRide

getMeterRidePrice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes)
getMeterRidePrice _merchantShortId _opCity rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  let tripStartTime = ride.tripStartTime
  let rideStatus = ride.status
  let driverId = ride.driverId
  merchantId <- fromMaybeM (RideNotFound rideId.getId) ride.merchantId
  let merchantOpCityId = ride.merchantOperatingCityId
  traveledDistance <- LU.getTravelledDistance driverId
  let waitingTime = case (ride.tripStartTime, ride.tripEndTime, ride.status) of
        (Just startTime, Just endTime, Domain.Types.Ride.COMPLETED) ->
          let estimatedSpeedMps = (20.0 * 1000) / 3600
              actualTimeDiffSeconds = realToFrac (diffUTCTime endTime startTime) :: Double
              distanceInMeters = realToFrac traveledDistance :: Double
              actualTimeDiffMinutes = actualTimeDiffSeconds / 60
              estimatedTimeMinutes = (distanceInMeters / estimatedSpeedMps) / 60
              waitMinutes = if actualTimeDiffMinutes - estimatedTimeMinutes > 0 then Just $ KTP.Minutes $ ceiling (actualTimeDiffMinutes - estimatedTimeMinutes) else Nothing
           in waitMinutes
        _ -> Nothing
  fareEstimates <- FC.calculateFareUtil merchantId merchantOpCityId Nothing (LatLong ride.fromLocation.lat ride.fromLocation.lon) (Just $ highPrecMetersToMeters traveledDistance) Nothing Nothing (OneWay MeterRide) waitingTime
  let mbMeterRideEstimate = Kernel.Prelude.listToMaybe fareEstimates.estimatedFares
  maybe
    (throwError . InternalError $ "Nahi aa rha hai fare :(" <> rideId.getId)
    ( \meterRideEstimate -> do
        return $ API.Types.UI.PriceBreakup.MeterRidePriceRes {fare = meterRideEstimate.minFare, distance = traveledDistance, tripStartTime = tripStartTime, status = Just rideStatus}
    )
    mbMeterRideEstimate
