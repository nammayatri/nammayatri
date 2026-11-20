{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Collect payment: shows the driver (and the rider) the fare the rider would pay if the ride
-- ended here, now -- without ending the ride.
--
-- Why: during the ride both apps show the estimate, and some drivers collect the estimate without
-- ending the ride, so the end-ride recompute (e.g. a toll on the estimate that was never crossed)
-- never reaches the rider. With this API the driver app hides the fare and calls this when the
-- driver wants to see it; the same number is pushed to the rider app.
--
-- How:
--   * The fare comes from 'RideEnd.previewRideFare', i.e. the exact end-ride fare computation run in
--     read-only 'RideEnd.PreviewMode' on the ride as it stands (distance and tolls tracked so far).
--   * The result is stored as a snapshot in ride.collected* (and its fare params row) and sent to the
--     BAP as an on_update RIDE_FARE_UPDATED.
--   * End ride is unchanged: it always recomputes and never reads the snapshot. ride.fare,
--     ride.fareParametersId and ride.status are never touched here.
--
-- Can be called any number of times while the ride is INPROGRESS; each call replaces the snapshot.
module Domain.Action.UI.Ride.CollectPayment
  ( CollectPaymentReq (..),
    CollectPaymentResp (..),
    CollectPaymentFareBreakupItem (..),
    collectPayment,
  )
where

import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Types.Booking as Fields
import qualified Domain.Types.FareParameters as Fields
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import qualified Environment as Fields
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.FareCalculator as Fare
import SharedLogic.Ride (validateRideLocation)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareParameters as QFare
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data CollectPaymentReq = CollectPaymentReq
  { -- | Driver's current location; used exactly like end ride's trip end point (pickup/drop threshold checks).
    point :: LatLong,
    -- | Current odometer reading. Required for odometer-billed trips (e.g. rentals), as in end ride.
    odometer :: Maybe DRide.OdometerReading
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CollectPaymentFareBreakupItem = CollectPaymentFareBreakupItem
  { title :: Text,
    price :: PriceAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CollectPaymentResp = CollectPaymentResp
  { -- | What the rider would pay if the ride ended now (same number end ride would bill at this point).
    fare :: PriceAPIEntity,
    fareBreakup :: [CollectPaymentFareBreakupItem],
    chargeableDistance :: Distance,
    -- | Toll included in 'fare' (Nothing = no toll).
    tollCharges :: Maybe PriceAPIEntity,
    -- | A manual toll declaration is required for this ride but the rider has not confirmed it yet, so
    -- 'fare' carries the automatic toll and may change at end ride. The driver app can start the
    -- existing end-ride/charge-approval flow.
    tollPendingConfirmation :: Bool,
    computedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Each call runs the full recompute (fare policy, congestion, maps on the failed-distance branch),
-- so it is rate limited per ride.
collectPaymentRateLimitOptions :: APIRateLimitOptions
collectPaymentRateLimitOptions = APIRateLimitOptions {limit = 10, limitResetTimeInSec = 60}

collectPayment ::
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DRide.Ride ->
  CollectPaymentReq ->
  Flow CollectPaymentResp
collectPayment (driverId, merchantId, _) rideId req = do
  checkSlidingWindowLimitWithOptions ("CollectPayment:rateLimit:" <> rideId.getId) collectPaymentRateLimitOptions

  -- Same request validation as end ride for a driver request.
  ride <- runInMasterDbAndRedis $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == driverId) $ throwError NotAnExecutor
  unless (ride.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus ("Payment can be collected only for a ride in progress: " <> show ride.status)
  validateRideLocation req.point
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  -- Same handle end ride builds (the ride's city, like the end-ride API); only its read-only parts are used in preview.
  endRideHandle <- RideEnd.buildEndRideHandle merchantId ride.merchantOperatingCityId (Just rideId) False
  preview <- RideEnd.previewRideFare endRideHandle booking ride req.point req.odometer
  computedAt <- getCurrentTime

  -- Persist the snapshot. A recomputed fare gets its own fare params row (never the booking's or the
  -- final one); otherwise the snapshot points at the booking's existing row, as end ride would.
  -- The ride update is conditional on INPROGRESS: if end ride completed the ride meanwhile, nothing
  -- is written and the rider is not notified (the BAP drops it on a completed ride too).
  whenJust preview.mbFareParamsToPersist QFare.create
  QRide.updateCollectedFare (Just preview.fare) (Just preview.fareParams.id) (Just preview.chargeableDistance) (Just computedAt) ride.id DRide.INPROGRESS
  updatedRide <- runInMasterDbAndRedis $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  if updatedRide.status == DRide.INPROGRESS
    then fork "collectPayment->sendRideFareUpdatedToBAP" $ CallBAP.sendRideFareUpdatedToBAP booking updatedRide preview.fare preview.fareParams computedAt preview.tollPendingConfirmation
    else logWarning $ "Ride ended while collecting payment, snapshot not shared with the rider. RideId: " <> rideId.getId

  logInfo $ "Collect payment for ride " <> rideId.getId <> ": fare " <> show preview.fare <> ", estimated fare " <> show booking.estimatedFare <> ", chargeable distance " <> show preview.chargeableDistance
  let mkPriceEntity amount = PriceAPIEntity {amount, currency = booking.currency}
  pure
    CollectPaymentResp
      { fare = mkPriceEntity preview.fare,
        fareBreakup = Fare.mkFareParamsBreakups True mkPriceEntity CollectPaymentFareBreakupItem preview.fareParams,
        chargeableDistance = convertMetersToDistance booking.distanceUnit preview.chargeableDistance,
        tollCharges = mkPriceEntity <$> preview.tollCharges,
        tollPendingConfirmation = preview.tollPendingConfirmation,
        computedAt
      }
