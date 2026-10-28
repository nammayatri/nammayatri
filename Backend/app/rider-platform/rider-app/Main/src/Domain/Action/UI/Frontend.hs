{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Frontend
  ( GetPersonFlowStatusRes,
    FrontendEvent (..),
    NotifyEventReq (..),
    NotifyEventResp,
    getPersonFlowStatus,
    notifyEvent,
  )
where

import Data.List (partition)
import qualified Data.Time as DT
import Domain.Action.UI.Booking
import qualified Domain.Action.UI.MultimodalConfirm as DMultimodal
import Domain.Action.UI.Quote
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingStatus as DB
import Domain.Types.CancellationReason
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Trip as DTrip
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyLeg.Types as JL
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.State.Types as JMState
import qualified SharedLogic.SilentReallocation as SilentRealloc
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as QNP
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus,
    isValueAddNP :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent,
    journeyId :: Maybe (Id DJ.Journey)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: Id DP.Person -> Id DM.Merchant -> Maybe Bool -> Maybe Bool -> Flow GetPersonFlowStatusRes
getPersonFlowStatus personId merchantId _ pollActiveBooking = do
  now <- getCurrentTime
  mbRiderConfig <- findRiderConfig personId
  let mbLiveJourneyWindow = toWindow (mbRiderConfig >>= (.flowStatusLiveJourneyWindowSec))
      mbFeedbackWindow = toWindow (mbRiderConfig >>= (.flowStatusFeedbackWindowSec))
      mbUnpaidJourneyWindow = toWindow (mbRiderConfig >>= (.flowStatusUnpaidJourneyWindowSec))
  activeJourneys <- QJourney.findAllActiveByRiderId personId
  processedActiveJourneys <- processJourneys now mbFeedbackWindow activeJourneys
  let (normalActiveRideJourneys, updatedActiveJourneys) = partition isNormalRideJourney processedActiveJourneys
  when (not (null normalActiveRideJourneys)) $ do
    fork "normalRideJourneys - Auto Fix" $ do
      forM_ normalActiveRideJourneys $ \normalRideJourney -> do
        mbJourneyLeg <- QJourneyLeg.findByJourneyIdAndSequenceNumber normalRideJourney.id 0
        whenJust (mbJourneyLeg >>= (.legSearchId)) $ \legSearchId -> do
          mbBooking <- QB.findByTransactionIdAndStatus legSearchId DB.terminalBookingStatus
          whenJust mbBooking $ \booking -> do
            case booking.status of
              DB.COMPLETED -> do
                void $ DMultimodal.postMultimodalOrderSublegSetTrackingStatus (Just personId, merchantId) normalRideJourney.id 0 1 JMState.Finished Nothing
                -- TODO :: For Backward compatibility, remove this post release
                void $ DMultimodal.postMultimodalOrderSublegSetStatus (Just personId, merchantId) normalRideJourney.id 0 1 JL.Completed
              DB.CANCELLED -> do
                void $ DMultimodal.postMultimodalOrderSublegSetTrackingStatus (Just personId, merchantId) normalRideJourney.id 0 1 JMState.Finished Nothing
                -- TODO :: For Backward compatibility, remove this post release
                void $ DMultimodal.postMultimodalOrderSublegSetStatus (Just personId, merchantId) normalRideJourney.id 0 1 JL.Cancelled
              DB.REALLOCATED -> do
                -- TODO :: Maybe Required to handle, please fix me !
                pure ()
              _ -> pure ()
  whenJust ((,) <$> mbUnpaidJourneyWindow <*> mbFeedbackWindow) $ \(unpaidJourneyWindow, feedbackWindow) ->
    fork "flowStatus - close stale journeys" $ do
      staleJourneys <- QJourney.findStaleActiveByRiderId personId now (DT.addUTCTime (negate unpaidJourneyWindow) now) (DT.addUTCTime (negate feedbackWindow) now) 5
      forM_ staleJourneys $ \journey -> JM.updateJourneyStatus journey (staleJourneyStatus journey)
  todaysJourneys <- case mbLiveJourneyWindow of
    Just liveJourneyWindow -> do
      let utcOffset = fromIntegral . getSeconds $ maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
          dayStart = DT.addUTCTime (negate utcOffset) (DT.UTCTime (DT.utctDay (DT.addUTCTime utcOffset now)) 0)
      QJourney.findTodaysPublicTransportByRiderId personId (DT.addUTCTime (negate liveJourneyWindow) now) dayStart (DT.addUTCTime DT.nominalDay dayStart)
    Nothing -> pure []
  mbFeedbackJourney <- case mbFeedbackWindow of
    Just feedbackWindow -> QJourney.findLatestFeedbackPendingByRiderId personId (DT.addUTCTime (negate feedbackWindow) now)
    Nothing -> pure Nothing
  scheduledJourneys <- processJourneys now mbFeedbackWindow (todaysJourneys <> maybeToList mbFeedbackJourney)
  let extraJourneys = filter (\journey -> journey.id `notElem` map (.id) updatedActiveJourneys) scheduledJourneys
      publicTransportJourneys = updatedActiveJourneys <> extraJourneys
      activeJourneysMode = not (null updatedActiveJourneys) && null normalActiveRideJourneys
  if activeJourneysMode
    then return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_JOURNEYS {journeys = publicTransportJourneys}) Nothing
    else showJourneysWhenIdle extraJourneys publicTransportJourneys <$> getRideFlowStatus now
  where
    getRideFlowStatus :: UTCTime -> Flow GetPersonFlowStatusRes
    getRideFlowStatus now = do
      personStatus' <- QPFS.getStatus personId
      case personStatus' of
        Just personStatus -> do
          case personStatus of
            DPFS.WAITING_FOR_DRIVER_OFFERS _ _ _ providerId _ _ -> do
              -- inside a silent reallocation window the app must keep its tracking screen,
              -- so report the reallocated booking as active instead of the pooling status
              mbSilentCtx <- SilentRealloc.getSilentReallocation personId
              case mbSilentCtx of
                Just _ -> do
                  activeBookings <- bookingList (Just personId, merchantId) Nothing False Nothing Nothing (Just True) Nothing Nothing Nothing Nothing [] Nothing Nothing
                  if null activeBookings.list
                    then findValueAddNP personStatus providerId now
                    else return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_BOOKINGS activeBookings.list) Nothing
                Nothing -> findValueAddNP personStatus providerId now
            DPFS.WAITING_FOR_DRIVER_ASSIGNMENT bookingId _ _ _ -> do
              mbBooking <- QB.findById bookingId
              case mbBooking of
                Just booking
                  | booking.status `notElem` DB.terminalBookingStatus ->
                    expirePersonStatusIfNeeded personStatus Nothing now
                _ -> checkForActiveBooking
            DPFS.WAITING_FOR_BOOKING_FEE_PAYMENT feeBookingId _ _ _ -> do
              mbFeeBooking <- QB.findById feeBookingId
              case mbFeeBooking of
                Just feeBooking
                  | feeBooking.status `notElem` DB.terminalBookingStatus ->
                    expirePersonStatusIfNeeded personStatus Nothing now
                _ -> checkForActiveBooking
            _ -> checkForActiveBooking
        Nothing -> checkForActiveBooking

    showJourneysWhenIdle :: [DJ.Journey] -> [DJ.Journey] -> GetPersonFlowStatusRes -> GetPersonFlowStatusRes
    showJourneysWhenIdle extraJourneys ptJourneys res = case res.currentStatus of
      DPFS.IDLE | not (null extraJourneys) -> GetPersonFlowStatusRes res.oldStatus (DPFS.ACTIVE_JOURNEYS {journeys = ptJourneys}) res.isValueAddNP
      _ -> res

    staleJourneyStatus :: DJ.Journey -> DJ.JourneyStatus
    staleJourneyStatus journey = case journey.status of
      DJ.FEEDBACK_PENDING -> DJ.COMPLETED
      DJ.CONFIRMED | journey.isPaymentSuccess /= Just True && journey.isPublicTransportIncluded == Just True -> DJ.FAILED
      _ -> DJ.EXPIRED

    checkForActiveBooking :: Flow GetPersonFlowStatusRes
    checkForActiveBooking = do
      if isJust pollActiveBooking
        then do
          activeBookings <- bookingList (Just personId, merchantId) Nothing False Nothing Nothing (Just True) Nothing Nothing Nothing Nothing [] Nothing Nothing
          if not (null activeBookings.list)
            then return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_BOOKINGS activeBookings.list) Nothing
            else do
              pendingFeedbackBookings <- bookingList (Just personId, merchantId) Nothing False (Just 1) Nothing (Just False) (Just DB.COMPLETED) Nothing Nothing Nothing [] Nothing Nothing
              case pendingFeedbackBookings.list of
                [booking] -> do
                  let isRated = isJust $ booking.rideList & listToMaybe >>= (.rideRating)
                  let isSkipped = fromMaybe True (booking.rideList & listToMaybe <&> (.feedbackSkipped))
                  if isRated || isSkipped
                    then return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
                    else return $ GetPersonFlowStatusRes Nothing (DPFS.FEEDBACK_PENDING booking) Nothing
                _ -> return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
        else return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing

    findValueAddNP personStatus providerId now = do
      isValueAddNP_ <- maybe (pure True) QNP.isValueAddNP providerId
      expirePersonStatusIfNeeded personStatus (Just isValueAddNP_) now

    expirePersonStatusIfNeeded personStatus isValueAddNp now = do
      if now < personStatus.validTill
        then return $ GetPersonFlowStatusRes Nothing personStatus isValueAddNp
        else do
          _ <- QPFS.updateStatus personId DPFS.IDLE
          return $ GetPersonFlowStatusRes (Just personStatus) DPFS.IDLE isValueAddNp

notifyEvent :: Id DP.Person -> Id DM.Merchant -> NotifyEventReq -> Flow NotifyEventResp
notifyEvent personId merchantId req = do
  _ <- case req.event of
    RATE_DRIVER_SKIPPED -> do
      QPFS.updateStatus personId DPFS.IDLE
      pendingFeedbackBookings <- bookingList (Just personId, merchantId) Nothing False (Just 1) Nothing (Just False) (Just DB.COMPLETED) Nothing Nothing Nothing [] Nothing Nothing
      case pendingFeedbackBookings.list of
        [booking] -> do
          let mbRideId = booking.rideList & listToMaybe <&> (.id)
          whenJust mbRideId $ \rideId -> QR.updateFeedbackSkipped True rideId
        _ -> pure ()
      mbRiderConfig <- findRiderConfig personId
      let mbFeedbackWindow = toWindow (mbRiderConfig >>= (.flowStatusFeedbackWindowSec))
      case req.journeyId of
        Just journeyId -> do
          QJourney.updateStatus DJ.COMPLETED journeyId
          fork "processOtherActiveJourneys" $ do
            now <- getCurrentTime
            activeJourneys <- QJourney.findAllActiveByRiderId personId
            processedActiveJourneys <- processJourneys now mbFeedbackWindow activeJourneys
            markJourneysComplete mbFeedbackWindow processedActiveJourneys
        -- TODO: For backward compatibility
        Nothing -> do
          activeJourneys <- QJourney.findAllActiveByRiderId personId
          markJourneysComplete mbFeedbackWindow activeJourneys
          whenJust mbFeedbackWindow $ \feedbackWindow -> do
            now <- getCurrentTime
            QJourney.findLatestFeedbackPendingByRiderId personId (DT.addUTCTime (negate feedbackWindow) now)
              >>= mapM_ (\journey -> JM.updateJourneyStatus journey DJ.COMPLETED)
    SEARCH_CANCELLED -> do
      activeBooking <- B.runInReplica $ QB.findLatestSelfAndPartyBookingByRiderId personId
      whenJust activeBooking $ \booking -> processActiveBooking booking Nothing OnSearch
      QPFS.updateStatus personId DPFS.IDLE
  pure APISuccess.Success
  where
    markJourneysComplete mbFeedbackWindow =
      mapM_
        ( \journey ->
            when (closesOnSkip mbFeedbackWindow journey) $
              JM.updateJourneyStatus journey DJ.COMPLETED
        )
    closesOnSkip :: Maybe DT.NominalDiffTime -> DJ.Journey -> Bool
    closesOnSkip mbFeedbackWindow journey = case mbFeedbackWindow of
      Just _ -> journey.status == DJ.FEEDBACK_PENDING || (isNormalRideJourney journey && isOpen)
      Nothing -> isOpen
      where
        isOpen = journey.status `notElem` [DJ.COMPLETED, DJ.EXPIRED, DJ.CANCELLED, DJ.FAILED]

isNormalRideJourney :: DJ.Journey -> Bool
isNormalRideJourney journey =
  case listToMaybe journey.modes of
    Just firstMode -> length journey.modes == 1 && (firstMode == DTrip.Taxi)
    Nothing -> False

findRiderConfig :: Id DP.Person -> Flow (Maybe DRC.RiderConfig)
findRiderConfig personId = do
  mbPerson <- QPerson.findById personId
  maybe (pure Nothing) (\person -> getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing) mbPerson

toWindow :: Maybe Seconds -> Maybe DT.NominalDiffTime
toWindow = fmap (fromIntegral . getSeconds)

-- filter payment success journeys and update journey status if expired
processJourneys :: UTCTime -> Maybe DT.NominalDiffTime -> [DJ.Journey] -> Flow [DJ.Journey]
processJourneys _ _ [] = return []
processJourneys now mbFeedbackWindow journeys = do
  updatedJourneys <- mapM updateJourneyStatus journeys
  return $ filter (\j -> j.status `notElem` [DJ.EXPIRED, DJ.COMPLETED]) updatedJourneys
  where
    updateJourneyStatus j
      | j.status == DJ.FEEDBACK_PENDING,
        Just feedbackWindow <- mbFeedbackWindow =
        if maybe False (< DT.addUTCTime (negate feedbackWindow) now) j.endTime
          then moveTo j DJ.COMPLETED
          else return j
      | otherwise =
        case j.journeyExpiryTime of
          Just expiryTime ->
            if now > expiryTime
              then moveTo j DJ.EXPIRED
              else return j
          Nothing -> return j
    moveTo j newStatus = do
      _ <- JM.updateJourneyStatus j newStatus
      return j {DJ.status = newStatus}
