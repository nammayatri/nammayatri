module Domain.Action.UI.Cancel
  ( cancel,
    CancelReq (..),
    CancelRes (..),
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QR
import Tools.Error

data CancelReq = CancelReq
  { reasonCode :: SCR.CancellationReasonCode,
    reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancelRes = CancelRes
  { bppBookingId :: Id SRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    cancellationSource :: SBCR.CancellationSource
  }

cancel :: (EncFlow m r, EsqDBFlow m r) => Id SRB.Booking -> Id Person.Person -> CancelReq -> m CancelRes
cancel bookingId _ req = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  canCancelBooking <- isBookingCancellable booking
  unless canCancelBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  when (booking.status == SRB.NEW) $ throwError (BookingInvalidStatus "NEW")
  bppBookingId <- fromMaybeM (BookingFieldNotPresent "bppBookingId") booking.bppBookingId

  duplicateCheck
  bookingCancelationReason <- buildBookingCancelationReason
  DB.runTransaction $ QBCR.create bookingCancelationReason
  return $
    CancelRes
      { bppBookingId = bppBookingId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        cancellationSource = SBCR.ByUser
      }
  where
    duplicateCheck = do
      rideBookingCancelationM <- QBCR.findByRideBookingId bookingId
      when (isJust rideBookingCancelationM) $
        throwError $ InvalidRequest "This ride is already cancelled."
    buildBookingCancelationReason = do
      let CancelReq {..} = req
      id <- generateGUID
      return $
        SBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            source = SBCR.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            ..
          }

isBookingCancellable :: EsqDBFlow m r => SRB.Booking -> m Bool
isBookingCancellable booking
  | booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT] = pure True
  | booking.status == SRB.TRIP_ASSIGNED = do
    ride <- QR.findActiveByRBId booking.id >>= fromMaybeM (RideDoesNotExist $ "BookingId: " <> booking.id.getId)
    pure (ride.status == Ride.NEW)
  | otherwise = pure False
