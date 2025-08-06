module Lib.JourneyModule.State.Utils where

import Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.RideStatus as DTaxiRide
import Domain.Types.RouteDetails as DRouteDetails
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyModule.State.Types
import Storage.Queries.Booking as QTaxiBooking
import Storage.Queries.Estimate as QTaxiEstimate
import Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.FRFSTicketBooking as QFRFSBooking
import Storage.Queries.FRFSTicketBookingFeedback as QFRFSTicketBookingFeedback
import Storage.Queries.JourneyLegsFeedbacks as SQJLFB
import Storage.Queries.Ride as QTaxiRide
import Storage.Queries.RouteDetails as QRouteDetails

getTaxiJourneyBookingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Maybe Text -> m TaxiJourneyLegStatus
getTaxiJourneyBookingStatus mbLegSearchId mbPricingId = do
  mbBooking <- maybe (return Nothing) (QTaxiBooking.findByTransactionId) mbLegSearchId
  mbRide <- maybe (return Nothing) (QTaxiRide.findActiveByRBId) (mbBooking <&> (.id))
  case mbRide of
    Just ride -> do
      case ride.status of
        DTaxiRide.COMPLETED ->
          case (ride.feedbackSkipped, ride.rideRating) of
            (True, _) -> return $ TaxiRide DTaxiRide.COMPLETED
            (_, Just _) -> return $ TaxiRide DTaxiRide.COMPLETED
            _ -> do
              -- TODO :: Below `SQJLFB` will be deprecated once UI is updated to send the feedback details
              result <-
                case (mbBooking >>= (.journeyId), mbBooking >>= (.journeyLegOrder)) of
                  (Just journeyId, Just legOrder) -> SQJLFB.findByJourneyIdAndLegOrder journeyId legOrder
                  _ -> return Nothing
              return $ if isNothing result then TaxiFeedback FEEDBACK_PENDING else TaxiRide DTaxiRide.COMPLETED
        rideStatus -> return $ TaxiRide rideStatus
    Nothing -> do
      case mbBooking of
        Just booking -> return $ TaxiBooking booking.status
        Nothing -> do
          mbEstimate <- maybe (return Nothing) (QTaxiEstimate.findById) (Id <$> mbPricingId)
          case mbEstimate of
            Just estimate -> return $ TaxiEstimate estimate.status
            Nothing -> return $ TaxiInitial EmptyParam

getFRFSJourneyBookingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Maybe Text -> m FRFSJourneyLegStatus
getFRFSJourneyBookingStatus mbLegSearchId _mbPricingId = do
  mbBooking <- maybe (return Nothing) (QFRFSBooking.findBySearchId) (Id <$> mbLegSearchId)
  case mbBooking of
    Just booking -> do
      case booking.status of
        DFRFSBooking.CONFIRMED -> do
          mbTickets <- QFRFSTicket.findAllByTicketBookingId booking.id
          let allTicketsVerified = all (\t -> t.status == DFRFSTicket.USED) mbTickets
              anyTicketActive = any (\t -> t.status == DFRFSTicket.ACTIVE) mbTickets
              allTicketExpired = any (\t -> t.status == DFRFSTicket.EXPIRED) mbTickets
              allTicketCancelled = all (\t -> t.status == DFRFSTicket.CANCELLED) mbTickets
          if allTicketsVerified
            then do
              isFeedbackGiven <-
                QFRFSTicketBookingFeedback.findByBookingId booking.id
                  >>= \case
                    Just _ -> return True
                    -- TODO :: Below `SQJLFB` will be deprecated once UI is updated to send the feedback details
                    Nothing -> case (booking.journeyId, booking.journeyLegOrder) of
                      (Just journeyId, Just legOrder) -> do
                        result <- SQJLFB.findByJourneyIdAndLegOrder journeyId legOrder
                        return $ isJust result
                      _ -> return False
              if isFeedbackGiven
                then return $ FRFSTicket DFRFSTicket.USED
                else return $ FRFSFeedback FEEDBACK_PENDING
            else
              if anyTicketActive
                then return $ FRFSTicket DFRFSTicket.ACTIVE
                else
                  if allTicketExpired
                    then return $ FRFSTicket DFRFSTicket.EXPIRED
                    else
                      if allTicketCancelled
                        then return $ FRFSTicket DFRFSTicket.CANCELLED
                        else return $ FRFSBooking DFRFSBooking.CONFIRMED
        bookingStatus -> return $ FRFSBooking bookingStatus
    Nothing -> return $ FRFSInitial EmptyParam

setJourneyLegTrackingStatus :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id DJourneyLeg.JourneyLeg -> Maybe Int -> TrackingStatus -> m ()
setJourneyLegTrackingStatus journeyLegId subLegOrder trackingStatus = do
  journeyRouteDetails <- QRouteDetails.findAllByJourneyLegId journeyLegId.getId
  let routeDetails =
        case subLegOrder of
          Just subLegOrder' ->
            case find (\rd -> maybe False (== subLegOrder') rd.subLegOrder) journeyRouteDetails of
              Just rd -> [rd]
              Nothing -> []
          Nothing -> journeyRouteDetails
  mapM_
    ( \rd -> do
        when (maybe True (trackingStatus >) rd.trackingStatus) $ do
          void $ QRouteDetails.updateTrackingStatus (Just trackingStatus) rd.id
    )
    routeDetails

getTaxiJourneyLegTrackingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Maybe Text -> DRouteDetails.RouteDetails -> m (Maybe TrackingStatus)
getTaxiJourneyLegTrackingStatus mbLegSearchId mbPricingId journeyRouteDetails = do
  taxiJourneyLegStatus <- getTaxiJourneyBookingStatus mbLegSearchId mbPricingId
  case taxiJourneyLegStatus of
    TaxiRide DTaxiRide.INPROGRESS -> do
      when (journeyRouteDetails.trackingStatus /= Just Ongoing) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Ongoing) journeyRouteDetails.id
      return $ Just Ongoing
    TaxiFeedback FEEDBACK_PENDING -> do
      when (journeyRouteDetails.trackingStatus /= Just FeedbackPending) $ do
        void $ QRouteDetails.updateTrackingStatus (Just FeedbackPending) journeyRouteDetails.id
      return $ Just FeedbackPending
    TaxiRide DTaxiRide.COMPLETED -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return $ Just Finished
    _ -> return journeyRouteDetails.trackingStatus

getFRFSJourneyLegTrackingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Maybe Text -> DRouteDetails.RouteDetails -> m (Maybe TrackingStatus)
getFRFSJourneyLegTrackingStatus mbLegSearchId mbPricingId journeyRouteDetails = do
  frfsJourneyLegStatus <- getFRFSJourneyBookingStatus mbLegSearchId mbPricingId
  case frfsJourneyLegStatus of
    FRFSFeedback FEEDBACK_PENDING -> do
      when (journeyRouteDetails.trackingStatus /= Just FeedbackPending) $ do
        void $ QRouteDetails.updateTrackingStatus (Just FeedbackPending) journeyRouteDetails.id
      return $ Just FeedbackPending
    FRFSBooking DFRFSBooking.CONFIRMED -> do
      when (journeyRouteDetails.trackingStatus /= Just Ongoing) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Ongoing) journeyRouteDetails.id
      return $ Just Ongoing
    FRFSTicket DFRFSTicket.ACTIVE -> do
      when (journeyRouteDetails.trackingStatus /= Just Ongoing) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Ongoing) journeyRouteDetails.id
      return $ Just Ongoing
    FRFSTicket DFRFSTicket.USED -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return $ Just Finished
    _ -> return journeyRouteDetails.trackingStatus

getWalkJourneyLegStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> Maybe Text -> DRouteDetails.RouteDetails -> m (Maybe TrackingStatus)
getWalkJourneyLegStatus _mbLegSearchId _mbPricingId journeyRouteDetails = do
  return journeyRouteDetails.trackingStatus
