module SharedLogic.Booking where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingStatus as DBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.JourneyLeg as QJL

getJourneyIdFromBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  DBooking.Booking ->
  m (Maybe (Id DJourney.Journey))
getJourneyIdFromBooking booking = do
  mbJourneyLeg <- QJL.findByLegSearchId (Just booking.transactionId)
  return $ mbJourneyLeg <&> (.journeyId)

getfareBreakups :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => DBooking.Booking -> Maybe DRide.Ride -> m ([DFareBreakup.FareBreakup], [DFareBreakup.FareBreakup])
getfareBreakups booking mRide = do
  case mRide of
    Just ride ->
      case booking.status of
        DBooking.COMPLETED -> do
          updatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE
          estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType booking.id.getId DFareBreakup.BOOKING
          let fareBreakups = if null updatedFareBreakups then estimatedFareBreakups else updatedFareBreakups
          pure (fareBreakups, estimatedFareBreakups)
        _ -> do
          --------- Need to remove it after fixing the status api polling in frontend ---------
          estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityTypeInKV booking.id.getId DFareBreakup.BOOKING
          pure ([], estimatedFareBreakups)
    Nothing -> do
      estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityTypeInKV booking.id.getId DFareBreakup.BOOKING
      pure ([], estimatedFareBreakups)
