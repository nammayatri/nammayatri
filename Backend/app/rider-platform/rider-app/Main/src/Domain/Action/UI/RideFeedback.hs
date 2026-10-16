-- | Ride feedback for the rider app, from booking confirmation to drop-off. Questions, answers and targeting
-- live in the driver platform (dynamic-offer-driver-app): these handlers check the rider owns the booking,
-- forward the call there with the driver platform's booking id (it works out the stage and the ride), and
-- run the actions an answer triggers (see SharedLogic.RideFeedback.Actions).
module Domain.Action.UI.RideFeedback
  ( getBookingFeedbackQuestions,
    postBookingFeedback,
  )
where

import qualified API.Types.UI.RideFeedback as API
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.AppFeedback.Cap as Cap
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.RideFeedback.Actions (ActionEnv (..), rideSubjectLines, runActionsAndReport)
import qualified SharedLogic.RideFeedback.Types as RFT
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-------------------------------------------------------------------------------
-- GET /booking/{bookingId}/feedback/questions

getBookingFeedbackQuestions ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DB.Booking ->
  Maybe Language ->
  Flow API.RideFeedbackQuestionsRes
getBookingFeedbackQuestions (mbPersonId, _) bookingId mbLanguage = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  booking <- fetchOwnedBooking personId bookingId
  merchant <- fetchMerchant booking
  now <- getCurrentTime
  -- Bookings served by this merchant's own driver platform, from confirmation until the ride ends.
  case (booking.bppBookingId, booking.status `elem` [DBS.CONFIRMED, DBS.AWAITING_REASSIGNMENT, DBS.TRIP_ASSIGNED]) of
    (Just bppBookingId, True) | servedByOwnDriverPlatform merchant booking -> do
      language <- resolveLanguage personId mbLanguage
      res <- CallBPPInternal.rideFeedbackQuestions merchant.driverOfferApiKey merchant.driverOfferBaseUrl bppBookingId.getId (Just language)
      questions <- decodeQuestions res.questions
      followUpQuestions <- decodeQuestions res.followUpQuestions
      pure API.RideFeedbackQuestionsRes {serverTime = res.serverTime, questions, followUpQuestions}
    _ -> pure API.RideFeedbackQuestionsRes {serverTime = now, questions = [], followUpQuestions = []}
  where
    decodeQuestions :: [A.Value] -> Flow [API.FeedbackQuestionItem]
    decodeQuestions = fmap catMaybes . mapM decodeQuestion
    decodeQuestion value = case A.fromJSON value of
      A.Success question -> pure (Just question)
      A.Error err -> do
        logWarning $ "RideFeedback: skipping a question this rider-app cannot read (" <> T.pack err <> ")"
        pure Nothing

-------------------------------------------------------------------------------
-- POST /booking/{bookingId}/feedback

postBookingFeedback ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DB.Booking ->
  API.SubmitRideFeedbackReq ->
  Flow API.SubmitRideFeedbackRes
postBookingFeedback (mbPersonId, _) bookingId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  booking <- fetchOwnedBooking personId bookingId
  merchant <- fetchMerchant booking
  bppBookingId <- case booking.bppBookingId of
    Just bppId | servedByOwnDriverPlatform merchant booking -> pure bppId.getId
    _ -> throwError (InvalidRequest "Ride feedback is not available for this booking")
  res <- CallBPPInternal.rideFeedbackSubmit merchant.driverOfferApiKey merchant.driverOfferBaseUrl bppBookingId req
  recordRideCardsShown personId req res
  let withActions = [r | r <- res.results, r.accepted, not (null r.actions)]
  unless (null withActions) $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    riderConfig <-
      getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
    forM_ withActions $ \result ->
      case (result.responseId, result.answer) of
        (Just responseId, Just answer) -> do
          ride <- maybe (pure Nothing) (QRide.findByBPPRideId . Id) result.rideId
          runActionsAndReport
            ActionEnv {merchant, riderConfig, person, ride, booking = Just booking, subjectLines = rideSubjectLines booking ride, questionKey = fromMaybe result.questionId result.questionKey, answer}
            bppBookingId
            responseId
            result.actions
        _ -> logError $ "RideFeedback: matched actions without a response or answer for question " <> result.questionId
  pure
    API.SubmitRideFeedbackRes
      { results =
          [ API.FeedbackSubmitResult {questionId = r.questionId, accepted = r.accepted, errorCode = r.errorCode}
            | r <- res.results
          ]
      }

-- | Ride feedback cards count toward the rider's feedback cap (SharedLogic.AppFeedback.Cap), so app
-- feedback does not follow right after one. Best-effort: the answers are already stored.
recordRideCardsShown :: Id DP.Person -> API.SubmitRideFeedbackReq -> RFT.SubmitFeedbackRes -> Flow ()
recordRideCardsShown personId req res = do
  now <- getCurrentTime
  let shownQuestions = [item.questionId | item <- req.responses, item.status == API.SHOWN]
  forM_ [responseId | r <- res.results, r.accepted, r.questionId `elem` shownQuestions, Just responseId <- [r.responseId]] $ \responseId ->
    void . withTryCatch "RideFeedback:recordShown" $ Cap.recordShown personId now ("ride-" <> responseId)

-------------------------------------------------------------------------------
-- Helpers

fetchOwnedBooking :: Id DP.Person -> Id DB.Booking -> Flow DB.Booking
fetchOwnedBooking personId bookingId = do
  booking <- QB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  unless (booking.riderId == personId) $ throwError (RideFeedbackAccessDenied bookingId.getId)
  pure booking

fetchMerchant :: DB.Booking -> Flow DM.Merchant
fetchMerchant booking = CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)

-- | The merchant's own driver platform registers as "<host>/beckn/<driverOfferMerchantId>"; bookings with
-- other providers (e.g. ONDC) have no ride feedback.
servedByOwnDriverPlatform :: DM.Merchant -> DB.Booking -> Bool
servedByOwnDriverPlatform merchant booking = merchant.driverOfferMerchantId `T.isSuffixOf` booking.providerId

resolveLanguage :: Id DP.Person -> Maybe Language -> Flow Language
resolveLanguage _ (Just language) = pure language
resolveLanguage personId Nothing = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pure $ fromMaybe ENGLISH person.language
