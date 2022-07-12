module Domain.Action.Beckn.OnUpdate (onUpdate, OnUpdateReq (..), OnUpdateFareBreakup (..)) where

import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Storage.Hedis.Config (HedisFlow)
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import EulerHS.Prelude hiding (state)
import ExternalAPI.Flow (BAPs, HasBapIds)
import qualified Product.Track as Track
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

data OnUpdateReq
  = RideAssignedReq
      { bppBookingId :: Id SRB.BPPRideBooking,
        bppRideId :: Id SRide.BPPRide,
        driverName :: Text,
        driverMobileNumber :: Text,
        driverRating :: Maybe Double,
        driverRegisteredAt :: UTCTime,
        otp :: Text,
        vehicleNumber :: Text,
        vehicleColor :: Text,
        vehicleModel :: Text
      }
  | RideStartedReq
      { bppBookingId :: Id SRB.BPPRideBooking,
        bppRideId :: Id SRide.BPPRide
      }
  | RideCompletedReq
      { bppBookingId :: Id SRB.BPPRideBooking,
        bppRideId :: Id SRide.BPPRide,
        fare :: Amount,
        totalFare :: Amount,
        fareBreakups :: [OnUpdateFareBreakup],
        chargeableDistance :: Double
      }
  | BookingCancelledReq
      { bppBookingId :: Id SRB.BPPRideBooking,
        cancellationSource :: SBCR.CancellationSource
      }
  | BookingReallocationReq
      { bppBookingId :: Id SRB.BPPRideBooking,
        bppRideId :: Id SRide.BPPRide
      }

data OnUpdateFareBreakup = OnUpdateFareBreakup
  { amount :: Amount,
    description :: Text
  }

onUpdate ::
  ( EsqDBFlow m r,
    FCMFlow m r,
    CoreMetrics m,
    HasBapIds c r m,
    HasFlowEnv
      m
      r
      '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HedisFlow m r
  ) =>
  OnUpdateReq ->
  m ()
onUpdate RideAssignedReq {..} = do
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideBookingId: " <> bppBookingId.getId)
  unless (isAssignable rideBooking) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- buildRide rideBooking
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.TRIP_ASSIGNED
    QRide.create ride
  Notify.notifyOnRideAssigned rideBooking ride
  Track.track ride.id
  where
    buildRide :: MonadFlow m => SRB.RideBooking -> m SRide.Ride
    buildRide rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            status = SRide.NEW,
            trackingUrl = Nothing,
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            vehicleVariant = rideBooking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            ..
          }
    isAssignable rideBooking = rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT]
onUpdate RideStartedReq {..} = do
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  DB.runTransaction $ do
    QRide.updateStatus ride.id SRide.INPROGRESS
  Notify.notifyOnRideStarted rideBooking ride
onUpdate RideCompletedReq {..} = do
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = Just chargeableDistance
            }
  breakups <- traverse (buildFareBreakup rideBooking.id) fareBreakups
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide
    QFareBreakup.createMany breakups
  Notify.notifyOnRideCompleted rideBooking ride
  where
    buildFareBreakup :: MonadFlow m => Id SRB.RideBooking -> OnUpdateFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup rideBookingId OnUpdateFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            ..
          }
onUpdate BookingCancelledReq {..} = do
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideBookingId: " <> bppBookingId.getId)
  unless (isRideBookingCancellable rideBooking) $
    throwError (RideBookingInvalidStatus (show rideBooking.status))
  mbRide <- QRide.findActiveByRBId rideBooking.id
  logTagInfo ("RideBookingId-" <> getId rideBooking.id) ("Cancellation reason " <> show cancellationSource)
  rideBookingCancellationReason <- buildRideBookingCancellationReason rideBooking.id (mbRide <&> (.id)) cancellationSource
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == SBCR.ByUser) $
      QBCR.create rideBookingCancellationReason
  -- notify customer
  Notify.notifyOnRideBookingCancelled rideBooking cancellationSource
  where
    isRideBookingCancellable rideBooking =
      rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
onUpdate BookingReallocationReq {..} = do
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  DB.runTransaction $ do
    QRB.updateStatus rideBooking.id SRB.AWAITING_REASSIGNMENT
    QRide.updateStatus ride.id SRide.CANCELLED
  -- notify customer
  Notify.notifyOnRideBookingReallocated rideBooking

buildRideBookingCancellationReason ::
  MonadFlow m =>
  Id SRB.RideBooking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  m SBCR.RideBookingCancellationReason
buildRideBookingCancellationReason rideBookingId mbRideId cancellationSource = do
  guid <- generateGUID
  return
    SBCR.RideBookingCancellationReason
      { id = guid,
        rideBookingId = rideBookingId,
        rideId = mbRideId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing
      }
