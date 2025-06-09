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

import Data.Ord (comparing)
import Domain.Action.UI.Booking
import qualified Domain.Action.UI.MultimodalConfirm as DMultimodal
import Domain.Action.UI.Quote
import qualified Domain.Types.Booking as DB
import Domain.Types.CancellationReason
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyModule.Base (generateJourneyInfoResponse, getAllLegsInfo)
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as QNP
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.Ride as QR

data GetPersonFlowStatusRes = GetPersonFlowStatusRes
  { oldStatus :: Maybe DPFS.FlowStatus,
    currentStatus :: DPFS.FlowStatus,
    isValueAddNP :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FrontendEvent = RATE_DRIVER_SKIPPED | SEARCH_CANCELLED
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype NotifyEventReq = NotifyEventReq
  { event :: FrontendEvent
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type NotifyEventResp = APISuccess

getPersonFlowStatus :: Id DP.Person -> Id DM.Merchant -> Maybe Bool -> Maybe Bool -> Flow GetPersonFlowStatusRes
getPersonFlowStatus personId merchantId _ pollActiveBooking = do
  activeJourneys <- DMultimodal.getActiveJourneyIds personId
  if not (null activeJourneys)
    then do
      let paymentSuccessJourneys = filter (\j -> j.isPaymentSuccess == Just True) activeJourneys
      if null paymentSuccessJourneys
        then return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_JOURNEYS {journeys = activeJourneys, currentJourney = Nothing}) Nothing
        else do
          let earliestActiveJourney = maximumBy (comparing (.startTime)) paymentSuccessJourneys
          legs <- getAllLegsInfo earliestActiveJourney.id False
          journeyInfoResp <- generateJourneyInfoResponse earliestActiveJourney legs
          return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_JOURNEYS {journeys = activeJourneys, currentJourney = Just journeyInfoResp}) Nothing
    else do
      personStatus' <- QPFS.getStatus personId
      case personStatus' of
        Just personStatus -> do
          case personStatus of
            DPFS.WAITING_FOR_DRIVER_OFFERS _ _ _ providerId _ -> findValueAddNP personStatus providerId
            DPFS.WAITING_FOR_DRIVER_ASSIGNMENT _ _ _ _ -> expirePersonStatusIfNeeded personStatus Nothing
            _ -> checkForActiveBooking
        Nothing -> checkForActiveBooking
  where
    checkForActiveBooking :: Flow GetPersonFlowStatusRes
    checkForActiveBooking = do
      if isJust pollActiveBooking
        then do
          activeBookings <- bookingList (personId, merchantId) Nothing Nothing (Just True) Nothing Nothing Nothing Nothing []
          if not (null activeBookings.list)
            then return $ GetPersonFlowStatusRes Nothing (DPFS.ACTIVE_BOOKINGS activeBookings.list) Nothing
            else do
              pendingFeedbackBookings <- bookingList (personId, merchantId) (Just 1) Nothing (Just False) (Just DB.COMPLETED) Nothing Nothing Nothing []
              case pendingFeedbackBookings.list of
                [booking] -> do
                  let isRated = isJust $ booking.rideList & listToMaybe >>= (.rideRating)
                  let isSkipped = fromMaybe True (booking.rideList & listToMaybe <&> (.feedbackSkipped))
                  if isRated || isSkipped
                    then return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
                    else return $ GetPersonFlowStatusRes Nothing (DPFS.FEEDBACK_PENDING booking) Nothing
                _ -> return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing
        else return $ GetPersonFlowStatusRes Nothing DPFS.IDLE Nothing

    findValueAddNP personStatus providerId = do
      isValueAddNP_ <- maybe (pure True) QNP.isValueAddNP providerId
      expirePersonStatusIfNeeded personStatus (Just isValueAddNP_)

    expirePersonStatusIfNeeded personStatus isValueAddNp = do
      now <- getCurrentTime
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
      pendingFeedbackBookings <- bookingList (personId, merchantId) (Just 1) Nothing (Just False) (Just DB.COMPLETED) Nothing Nothing Nothing []
      case pendingFeedbackBookings.list of
        [booking] -> do
          let mbRideId = booking.rideList & listToMaybe <&> (.id)
          whenJust mbRideId $ \rideId -> QR.updateFeedbackSkipped True rideId
        _ -> pure ()
      activeJourneys <- DMultimodal.getActiveJourneyIds personId
      mapM_ (QJourney.updateStatus DJ.COMPLETED) (activeJourneys <&> (.id))
    SEARCH_CANCELLED -> do
      activeBooking <- B.runInReplica $ QB.findLatestSelfAndPartyBookingByRiderId personId
      whenJust activeBooking $ \booking -> processActiveBooking booking OnSearch
      QPFS.updateStatus personId DPFS.IDLE
  pure APISuccess.Success
