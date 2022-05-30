module Domain.Action.UI.Cancel
  ( cancel,
    CancelReq (..),
    CancelRes (..),
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Id
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideBookingCancellationReason as QBCR
import Types.Error
import Utils.Common

data CancelReq = CancelReq
  { reasonCode :: SCR.CancellationReasonCode,
    reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancelRes = CancelRes
  { bppBookingId :: Id SRB.BPPRideBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    cancellationSource :: SBCR.CancellationSource
  }

cancel :: (EncFlow m r, EsqDBFlow m r) => Id SRB.RideBooking -> Id Person.Person -> CancelReq -> m CancelRes
cancel bookingId _ req = do
  rideBooking <- QRB.findById bookingId >>= fromMaybeM (RideBookingDoesNotExist bookingId.getId)
  canCancelRideBooking <- isRideBookingCancellable rideBooking
  unless canCancelRideBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  when (rideBooking.status == SRB.NEW) $ throwError (RideBookingInvalidStatus "NEW")
  bppBookingId <- fromMaybeM (RideBookingFieldNotPresent "bppBookingId") rideBooking.bppBookingId

  rideBookingCancelationReason <- buildRideBookingCancelationReason rideBooking.id
  DB.runTransaction
    (QBCR.create rideBookingCancelationReason)
    `rethrow` \(SQLRequestError _ _) -> RideInvalidStatus "This ride is already cancelled"
  return $
    CancelRes
      { bppBookingId = bppBookingId,
        bppId = rideBooking.providerId,
        bppUrl = rideBooking.providerUrl,
        cancellationSource = SBCR.ByUser
      }
  where
    buildRideBookingCancelationReason rideBookingId = do
      let CancelReq {..} = req
      id <- generateGUID
      return $
        SBCR.RideBookingCancellationReason
          { rideBookingId = rideBookingId,
            rideId = Nothing,
            source = SBCR.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            ..
          }

isRideBookingCancellable :: EsqDBFlow m r => SRB.RideBooking -> m Bool
isRideBookingCancellable rideBooking
  | rideBooking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT] = pure True
  | rideBooking.status == SRB.TRIP_ASSIGNED = do
    ride <- QR.findActiveByRBId rideBooking.id >>= fromMaybeM (RideDoesNotExist $ "BookingId: " <> rideBooking.id.getId)
    pure (ride.status == Ride.NEW)
  | otherwise = pure False
