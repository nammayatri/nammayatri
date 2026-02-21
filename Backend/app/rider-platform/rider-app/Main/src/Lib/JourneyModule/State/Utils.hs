module Lib.JourneyModule.State.Utils where

import Domain.Types.Booking as DBooking
import Domain.Types.BookingStatus as DTaxiBooking
import Domain.Types.Estimate as DEstimate
import Domain.Types.EstimateStatus as DTaxiEstimate
import Domain.Types.FRFSTicketBooking as DFRFSBooking
import Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Ride as DRide
import Domain.Types.RideStatus as DTaxiRide
import Domain.Types.RouteDetails as DRouteDetails
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLTypes
import Lib.JourneyModule.State.Types
import Storage.Queries.FRFSTicket as QFRFSTicket
import Storage.Queries.FRFSTicketBookingFeedback as QFRFSTicketBookingFeedback
import Storage.Queries.RouteDetails as QRouteDetails
import Storage.Queries.RouteDetailsExtra as QRouteDetailsExtra

getFRFSAllStatuses :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe DFRFSBooking.FRFSTicketBooking -> m (JLTypes.JourneyLegStatus, JourneyBookingStatus, [(Int, TrackingStatus, UTCTime)])
getFRFSAllStatuses journeyLeg mbBooking = do
  now <- getCurrentTime
  bookingStatus <- getFRFSJourneyBookingStatus mbBooking
  trackingStatuses <-
    mapM
      ( \routeDetail -> do
          let trackingStatus = getFRFSJourneyLegTrackingStatus mbBooking routeDetail
          return (fromMaybe 1 routeDetail.subLegOrder, trackingStatus, fromMaybe now routeDetail.trackingStatusLastUpdatedAt)
      )
      journeyLeg.routeDetails
  let oldStatus =
        if not (null trackingStatuses) && all (\(_, trackingStatus, _) -> trackingStatus == Finished) trackingStatuses
          then JLTypes.Completed
          else
            if bookingStatus `elem` [FRFSTicket DFRFSTicket.CANCELLED, FRFSBooking DFRFSBooking.CANCELLED]
              then JLTypes.Skipped
              else case bookingStatus of
                FRFSBooking status -> getFRFSLegStatusFromBooking status
                FRFSTicket DFRFSTicket.CANCELLED -> JLTypes.Cancelled
                FRFSTicket DFRFSTicket.USED -> JLTypes.Completed
                FRFSTicket DFRFSTicket.EXPIRED -> JLTypes.Completed
                Feedback _ -> JLTypes.Completed
                _ -> castTrackingStatusToJourneyLegStatus (fromMaybe InPlan (listToMaybe trackingStatuses <&> (\(_, a, _) -> a))) -- for UI backward compatibility
  return (oldStatus, bookingStatus, trackingStatuses)
  where
    getFRFSLegStatusFromBooking :: DFRFSBooking.FRFSTicketBookingStatus -> JLTypes.JourneyLegStatus
    getFRFSLegStatusFromBooking bookingStatus = case bookingStatus of
      DFRFSBooking.NEW -> JLTypes.InPlan
      DFRFSBooking.APPROVED -> JLTypes.InPlan
      DFRFSBooking.PAYMENT_PENDING -> JLTypes.InPlan
      DFRFSBooking.CONFIRMING -> JLTypes.Assigning
      DFRFSBooking.CONFIRMED -> JLTypes.Booked
      DFRFSBooking.FAILED -> JLTypes.Failed
      DFRFSBooking.CANCELLED -> JLTypes.Cancelled
      DFRFSBooking.COUNTER_CANCELLED -> JLTypes.Cancelled
      DFRFSBooking.CANCEL_INITIATED -> JLTypes.Cancelled
      DFRFSBooking.TECHNICAL_CANCEL_REJECTED -> JLTypes.InPlan

getWalkAllStatuses :: DJourneyLeg.JourneyLeg -> (JLTypes.JourneyLegStatus, TrackingStatus, Maybe UTCTime)
getWalkAllStatuses journeyLeg = do
  case listToMaybe journeyLeg.routeDetails of
    Just routeDetail -> (maybe JLTypes.InPlan castTrackingStatusToJourneyLegStatus routeDetail.trackingStatus, fromMaybe InPlan routeDetail.trackingStatus, routeDetail.trackingStatusLastUpdatedAt)
    Nothing -> (JLTypes.InPlan, InPlan, Nothing)

getTaxiAllStatuses :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe DEstimate.Estimate -> m (JLTypes.JourneyLegStatus, JourneyBookingStatus, TrackingStatus, UTCTime)
getTaxiAllStatuses journeyLeg mbBooking mbRide mbEstimate = do
  now <- getCurrentTime
  let bookingStatus = getTaxiJourneyBookingStatus mbBooking mbRide mbEstimate
  trackingStatus <- maybe (return InPlan) (getTaxiJourneyLegTrackingStatus bookingStatus) (listToMaybe journeyLeg.routeDetails)
  let oldStatus =
        if trackingStatus == Finished
          then JLTypes.Completed
          else do
            -- If status is completed, a booking should exist. If it appears here without a booking, it means the booking was cancelled.
            if bookingStatus `elem` [TaxiRide DTaxiRide.CANCELLED, TaxiBooking DTaxiBooking.CANCELLED, TaxiEstimate DTaxiEstimate.CANCELLED, TaxiEstimate DTaxiEstimate.COMPLETED]
              then JLTypes.Skipped
              else do
                case bookingStatus of
                  TaxiEstimate status -> mapTaxiEstimateStatusToJourneyLegStatus status
                  TaxiBooking status -> mapTaxiBookingStatusToJourneyLegStatus status
                  TaxiRide status -> fromMaybe (castTrackingStatusToJourneyLegStatus trackingStatus) (mapTaxiRideStatusToJourneyLegStatus status)
                  Feedback _ -> JLTypes.Completed
                  _ -> castTrackingStatusToJourneyLegStatus trackingStatus -- for UI backward compatibility
  return (oldStatus, bookingStatus, trackingStatus, fromMaybe now ((listToMaybe journeyLeg.routeDetails) >>= (.trackingStatusLastUpdatedAt)))
  where
    mapTaxiEstimateStatusToJourneyLegStatus :: DTaxiEstimate.EstimateStatus -> JLTypes.JourneyLegStatus
    mapTaxiEstimateStatusToJourneyLegStatus estimateStatus =
      case estimateStatus of
        DTaxiEstimate.NEW -> JLTypes.InPlan
        DTaxiEstimate.COMPLETED -> JLTypes.Cancelled -- If status is completed, a booking should exist. If it appears here without a booking, it means the booking was cancelled.
        DTaxiEstimate.CANCELLED -> JLTypes.Cancelled
        _ -> JLTypes.Assigning

    mapTaxiRideStatusToJourneyLegStatus :: DTaxiRide.RideStatus -> Maybe JLTypes.JourneyLegStatus
    mapTaxiRideStatusToJourneyLegStatus status = case status of
      DTaxiRide.UPCOMING -> Just JLTypes.InPlan
      DTaxiRide.COMPLETED -> Just JLTypes.Completed
      DTaxiRide.CANCELLED -> Just JLTypes.Cancelled
      _ -> Nothing

    mapTaxiBookingStatusToJourneyLegStatus :: DTaxiBooking.BookingStatus -> JLTypes.JourneyLegStatus
    mapTaxiBookingStatusToJourneyLegStatus status = case status of
      DTaxiBooking.NEW -> JLTypes.InPlan
      DTaxiBooking.CONFIRMED -> JLTypes.InPlan
      DTaxiBooking.AWAITING_REASSIGNMENT -> JLTypes.Assigning
      DTaxiBooking.REALLOCATED -> JLTypes.Cancelled
      DTaxiBooking.COMPLETED -> JLTypes.Completed
      DTaxiBooking.CANCELLED -> JLTypes.Cancelled
      DTaxiBooking.TRIP_ASSIGNED -> JLTypes.Booked

getTaxiJourneyBookingStatus :: Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe DEstimate.Estimate -> JourneyBookingStatus
getTaxiJourneyBookingStatus mbBooking mbRide mbEstimate = do
  case mbRide of
    Just ride -> do
      case ride.status of
        DTaxiRide.COMPLETED ->
          case (ride.feedbackSkipped, ride.rideRating) of
            (True, _) -> TaxiRide DTaxiRide.COMPLETED
            (_, Just _) -> TaxiRide DTaxiRide.COMPLETED
            _ -> Feedback FEEDBACK_PENDING
        rideStatus -> TaxiRide rideStatus
    Nothing -> do
      case mbBooking of
        Just booking -> TaxiBooking booking.status
        Nothing -> do
          case mbEstimate of
            Just estimate -> TaxiEstimate estimate.status
            Nothing -> Initial BOOKING_PENDING

getFRFSJourneyBookingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe DFRFSBooking.FRFSTicketBooking -> m JourneyBookingStatus
getFRFSJourneyBookingStatus mbBooking = do
  case mbBooking of
    Just booking -> do
      case booking.status of
        DFRFSBooking.CONFIRMED -> do
          mbTickets <- QFRFSTicket.findAllByTicketBookingId booking.id
          let allTicketsCheckedOut = all (\t -> t.status == DFRFSTicket.USED) mbTickets
              allTicketsCheckedIn = all (\t -> t.status == DFRFSTicket.INPROGRESS) mbTickets
              anyTicketActive = any (\t -> t.status == DFRFSTicket.ACTIVE) mbTickets
              allTicketExpired = any (\t -> t.status == DFRFSTicket.EXPIRED) mbTickets
              allTicketCancelled = all (\t -> t.status == DFRFSTicket.CANCELLED) mbTickets
          if allTicketsCheckedOut
            then do
              isFeedbackGiven <-
                QFRFSTicketBookingFeedback.findByBookingId booking.id
                  >>= \case
                    Just _ -> return True
                    Nothing -> return False
              if isFeedbackGiven
                then return (FRFSTicket DFRFSTicket.USED)
                else return (Feedback FEEDBACK_PENDING)
            else
              if anyTicketActive
                then return (FRFSTicket DFRFSTicket.ACTIVE)
                else
                  if allTicketsCheckedIn
                    then return (FRFSTicket DFRFSTicket.INPROGRESS)
                    else
                      if allTicketExpired
                        then return (FRFSTicket DFRFSTicket.EXPIRED)
                        else
                          if allTicketCancelled
                            then return (FRFSTicket DFRFSTicket.CANCELLED)
                            else return (FRFSBooking DFRFSBooking.CONFIRMED)
        bookingStatus -> return (FRFSBooking bookingStatus)
    Nothing -> return (Initial BOOKING_PENDING)

setJourneyLegTrackingStatus :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe Int -> TrackingStatus -> UTCTime -> m ()
setJourneyLegTrackingStatus journeyLeg subLegOrder trackingStatus trackingStatusUpdateTime = do
  let routeDetails =
        case subLegOrder of
          Just subLegOrder' ->
            case find (\rd -> maybe False (== subLegOrder') rd.subLegOrder) journeyLeg.routeDetails of
              Just rd -> [rd]
              Nothing -> []
          Nothing -> journeyLeg.routeDetails
  mapM_
    ( \rd -> do
        -- Removed this validation as in case of Manual Fix Location, Tracking Status can go back in Past.
        -- when (maybe True (trackingStatus >) rd.trackingStatus) $ do
        void $ QRouteDetailsExtra.updateTrackingStatusWithTime (Just trackingStatus) rd.id trackingStatusUpdateTime
    )
    routeDetails

getTaxiJourneyLegTrackingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => JourneyBookingStatus -> DRouteDetails.RouteDetails -> m TrackingStatus
getTaxiJourneyLegTrackingStatus taxiJourneyLegStatus journeyRouteDetails = do
  let shouldResetToInPlan = \case
        Initial _ -> True
        TaxiEstimate _ -> True
        TaxiBooking _ -> True
        TaxiRide DTaxiRide.CANCELLED -> True
        _ -> False
  case taxiJourneyLegStatus of
    TaxiRide DTaxiRide.INPROGRESS -> do
      when (journeyRouteDetails.trackingStatus /= Just Ongoing) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Ongoing) journeyRouteDetails.id
      return Ongoing
    TaxiRide DTaxiRide.COMPLETED -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return Finished
    Feedback FEEDBACK_PENDING -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return Finished
    bookingStatus
      | shouldResetToInPlan bookingStatus -> do
        -- handles cancellation and find another driver for taxi leg in journey.
        when (journeyRouteDetails.trackingStatus /= Just InPlan) $
          void $ QRouteDetails.updateTrackingStatus (Just InPlan) journeyRouteDetails.id
        return InPlan
      | otherwise -> return (fromMaybe InPlan journeyRouteDetails.trackingStatus)

getFRFSJourneyLegTrackingStatus :: Maybe DFRFSBooking.FRFSTicketBooking -> DRouteDetails.RouteDetails -> TrackingStatus
getFRFSJourneyLegTrackingStatus _mbBooking journeyRouteDetails = fromMaybe InPlan journeyRouteDetails.trackingStatus

castTrackingStatusToJourneyLegStatus :: TrackingStatus -> JLTypes.JourneyLegStatus
castTrackingStatusToJourneyLegStatus = \case
  InPlan -> JLTypes.InPlan
  Arriving -> JLTypes.OnTheWay
  AlmostArrived -> JLTypes.Arriving
  Arrived -> JLTypes.Arrived
  Ongoing -> JLTypes.Ongoing
  Finishing -> JLTypes.Finishing
  ExitingStation -> JLTypes.Finishing
  Finished -> JLTypes.Completed
