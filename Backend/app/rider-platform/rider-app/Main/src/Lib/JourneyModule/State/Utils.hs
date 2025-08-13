module Lib.JourneyModule.State.Utils where

import Domain.Types.Booking as DBooking
import Domain.Types.Estimate as DEstimate
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

getFRFSAllStatuses :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe DFRFSBooking.FRFSTicketBooking -> m (JLTypes.JourneyLegStatus, Maybe JourneyBookingStatus, [(Int, Maybe TrackingStatus)])
getFRFSAllStatuses journeyLeg mbBooking = do
  bookingStatus <- getFRFSJourneyBookingStatus mbBooking
  trackingStatuses <-
    mapM
      ( \routeDetail -> do
          trackingStatus <- getFRFSJourneyLegTrackingStatus mbBooking routeDetail
          return (fromMaybe 1 routeDetail.subLegOrder, trackingStatus)
      )
      journeyLeg.routeDetails
  let oldStatus =
        if journeyLeg.isSkipped == Just True
          then JLTypes.Skipped
          else maybe JLTypes.InPlan castTrackingStatusToJourneyLegStatus ((listToMaybe trackingStatuses) >>= snd) -- for UI backward compatibility
  return (oldStatus, bookingStatus, trackingStatuses)

getWalkAllStatuses :: DJourneyLeg.JourneyLeg -> (JLTypes.JourneyLegStatus, Maybe TrackingStatus)
getWalkAllStatuses journeyLeg = do
  case (listToMaybe journeyLeg.routeDetails) of
    Just routeDetail -> (maybe JLTypes.InPlan castTrackingStatusToJourneyLegStatus routeDetail.trackingStatus, routeDetail.trackingStatus)
    Nothing -> (JLTypes.InPlan, Nothing)

getTaxiAllStatuses :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe DEstimate.Estimate -> m (JLTypes.JourneyLegStatus, Maybe JourneyBookingStatus, Maybe TrackingStatus)
getTaxiAllStatuses journeyLeg mbBooking mbRide mbEstimate = do
  let bookingStatus = getTaxiJourneyBookingStatus mbBooking mbRide mbEstimate
  mbTrackingStatus <- getTaxiJourneyLegTrackingStatus mbBooking mbRide mbEstimate (listToMaybe journeyLeg.routeDetails)
  let oldStatus =
        if journeyLeg.isSkipped == Just True
          then JLTypes.Skipped
          else maybe JLTypes.InPlan castTrackingStatusToJourneyLegStatus mbTrackingStatus -- for UI backward compatibility
  return (oldStatus, bookingStatus, mbTrackingStatus)

getTaxiJourneyBookingStatus :: Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe DEstimate.Estimate -> Maybe JourneyBookingStatus
getTaxiJourneyBookingStatus mbBooking mbRide mbEstimate = do
  case mbRide of
    Just ride -> do
      case ride.status of
        DTaxiRide.COMPLETED ->
          case (ride.feedbackSkipped, ride.rideRating) of
            (True, _) -> Just (TaxiRide DTaxiRide.COMPLETED)
            (_, Just _) -> Just (TaxiRide DTaxiRide.COMPLETED)
            _ -> Just (Feedback FEEDBACK_PENDING)
        rideStatus -> Just (TaxiRide rideStatus)
    Nothing -> do
      case mbBooking of
        Just booking -> Just (TaxiBooking booking.status)
        Nothing -> do
          case mbEstimate of
            Just estimate -> Just (TaxiEstimate estimate.status)
            Nothing -> Nothing

getFRFSJourneyBookingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe DFRFSBooking.FRFSTicketBooking -> m (Maybe JourneyBookingStatus)
getFRFSJourneyBookingStatus mbBooking = do
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
                    Nothing -> return False
              if isFeedbackGiven
                then return $ Just (FRFSTicket DFRFSTicket.USED)
                else return $ Just (Feedback FEEDBACK_PENDING)
            else
              if anyTicketActive
                then return $ Just (FRFSTicket DFRFSTicket.ACTIVE)
                else
                  if allTicketExpired
                    then return $ Just (FRFSTicket DFRFSTicket.EXPIRED)
                    else
                      if allTicketCancelled
                        then return $ Just (FRFSTicket DFRFSTicket.CANCELLED)
                        else return $ Just (FRFSBooking DFRFSBooking.CONFIRMED)
        bookingStatus -> return $ Just (FRFSBooking bookingStatus)
    Nothing -> return Nothing

setJourneyLegTrackingStatus :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DJourneyLeg.JourneyLeg -> Maybe Int -> TrackingStatus -> m ()
setJourneyLegTrackingStatus journeyLeg subLegOrder trackingStatus = do
  let routeDetails =
        case subLegOrder of
          Just subLegOrder' ->
            case find (\rd -> maybe False (== subLegOrder') rd.subLegOrder) journeyLeg.routeDetails of
              Just rd -> [rd]
              Nothing -> []
          Nothing -> journeyLeg.routeDetails
  mapM_
    ( \rd -> do
        when (maybe True (trackingStatus >) rd.trackingStatus) $ do
          void $ QRouteDetails.updateTrackingStatus (Just trackingStatus) rd.id
    )
    routeDetails

getTaxiJourneyLegTrackingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe DEstimate.Estimate -> Maybe DRouteDetails.RouteDetails -> m (Maybe TrackingStatus)
getTaxiJourneyLegTrackingStatus _ _ _ Nothing = return Nothing
getTaxiJourneyLegTrackingStatus mbBooking mbRide mbEstimate (Just journeyRouteDetails) = do
  let taxiJourneyLegStatus = getTaxiJourneyBookingStatus mbBooking mbRide mbEstimate
  case taxiJourneyLegStatus of
    Just (TaxiRide DTaxiRide.INPROGRESS) -> do
      when (journeyRouteDetails.trackingStatus /= Just Ongoing) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Ongoing) journeyRouteDetails.id
      return $ Just Ongoing
    Just (TaxiRide DTaxiRide.COMPLETED) -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return $ Just Finished
    _ -> return journeyRouteDetails.trackingStatus

getFRFSJourneyLegTrackingStatus :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe DFRFSBooking.FRFSTicketBooking -> DRouteDetails.RouteDetails -> m (Maybe TrackingStatus)
getFRFSJourneyLegTrackingStatus mbBooking journeyRouteDetails = do
  frfsJourneyLegStatus <- getFRFSJourneyBookingStatus mbBooking
  case frfsJourneyLegStatus of
    Just (FRFSTicket DFRFSTicket.USED) -> do
      when (journeyRouteDetails.trackingStatus /= Just Finished) $ do
        void $ QRouteDetails.updateTrackingStatus (Just Finished) journeyRouteDetails.id
      return $ Just Finished
    _ -> return journeyRouteDetails.trackingStatus

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
