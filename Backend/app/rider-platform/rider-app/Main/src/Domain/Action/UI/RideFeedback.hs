-- | During-ride feedback for the rider app. Questions, answers and targeting live in the driver platform
-- (dynamic-offer-driver-app): these handlers check the rider owns the ride, forward the call there with
-- the driver platform's ride id, and run the actions an answer triggers (see SharedLogic.RideFeedback.Actions).
module Domain.Action.UI.RideFeedback
  ( getRideFeedbackQuestions,
    postRideFeedback,
    getRideFeedback,
  )
where

import qualified API.Types.UI.RideFeedback as API
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRS
import Environment
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.RideFeedback.Actions (ActionEnv (..), runActionsAndReport)
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-------------------------------------------------------------------------------
-- GET /ride/{rideId}/feedback/questions

getRideFeedbackQuestions ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRide.Ride ->
  Maybe Language ->
  Flow API.RideFeedbackQuestionsRes
getRideFeedbackQuestions (mbPersonId, _) rideId mbLanguage = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  (ride, booking) <- fetchOwnedRide personId rideId
  merchant <- fetchMerchant booking
  now <- getCurrentTime
  -- Only rides served by this merchant's own driver platform, while the ride is assigned or on.
  if ride.status `notElem` [DRS.NEW, DRS.INPROGRESS] || not (servedByOwnDriverPlatform merchant booking)
    then pure API.RideFeedbackQuestionsRes {rideId, serverTime = now, questions = [], followUpQuestions = []}
    else do
      language <- resolveLanguage personId mbLanguage
      res <- CallBPPInternal.rideFeedbackQuestions merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId (Just language)
      questions <- decodeQuestions res.questions
      followUpQuestions <- decodeQuestions res.followUpQuestions
      -- The driver platform answers with its own ride id; the rider app knows this ride by ours.
      pure API.RideFeedbackQuestionsRes {rideId, serverTime = res.serverTime, questions, followUpQuestions}
  where
    decodeQuestions :: [A.Value] -> Flow [API.FeedbackQuestionItem]
    decodeQuestions = fmap catMaybes . mapM decodeQuestion
    decodeQuestion value = case A.fromJSON value of
      A.Success question -> pure (Just question)
      A.Error err -> do
        logWarning $ "RideFeedback: skipping a question this rider-app cannot read (" <> T.pack err <> ")"
        pure Nothing

-------------------------------------------------------------------------------
-- POST /ride/{rideId}/feedback

postRideFeedback ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRide.Ride ->
  Maybe Language ->
  API.SubmitRideFeedbackReq ->
  Flow API.SubmitRideFeedbackRes
postRideFeedback (mbPersonId, _) rideId mbLanguage req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  (ride, booking) <- fetchOwnedRide personId rideId
  merchant <- fetchMerchant booking
  unless (servedByOwnDriverPlatform merchant booking) $
    throwError (InvalidRequest "During-ride feedback is not available for this ride")
  language <- resolveLanguage personId mbLanguage
  res <- CallBPPInternal.rideFeedbackSubmit merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId (Just language) req
  let withActions = [r | r <- res.results, r.accepted, not (null r.actions)]
  unless (null withActions) $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    riderConfig <-
      getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
    forM_ withActions $ \result ->
      case (result.responseId, answerFor result.questionId) of
        (Just responseId, Just answer) ->
          runActionsAndReport
            ActionEnv {merchant, riderConfig, person, ride, booking, questionKey = fromMaybe result.questionId result.questionKey, answer}
            responseId
            result.actions
        _ -> logError $ "RideFeedback: matched actions without a response or answer for question " <> result.questionId
  pure
    API.SubmitRideFeedbackRes
      { results =
          [ API.FeedbackSubmitResult {questionId = r.questionId, accepted = r.accepted, errorCode = r.errorCode, acknowledgement = r.acknowledgement}
            | r <- res.results
          ]
      }
  where
    -- A batch can carry several items for one question (SHOWN, then ANSWERED); the actions go with its answer.
    answerFor questionId = listToMaybe (reverse [answer | item <- req.responses, item.questionId == questionId, Just answer <- [item.answer]])

-------------------------------------------------------------------------------
-- GET /ride/{rideId}/feedback

getRideFeedback :: (Maybe (Id DP.Person), Id DM.Merchant) -> Id DRide.Ride -> Flow API.RideFeedbackSubmittedRes
getRideFeedback (mbPersonId, _) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  (ride, booking) <- fetchOwnedRide personId rideId
  merchant <- fetchMerchant booking
  if servedByOwnDriverPlatform merchant booking
    then CallBPPInternal.rideFeedbackSubmitted merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId
    else pure API.RideFeedbackSubmittedRes {responses = []}

-------------------------------------------------------------------------------
-- Helpers

fetchOwnedRide :: Id DP.Person -> Id DRide.Ride -> Flow (DRide.Ride, DB.Booking)
fetchOwnedRide personId rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError (RideFeedbackAccessDenied rideId.getId)
  pure (ride, booking)

fetchMerchant :: DB.Booking -> Flow DM.Merchant
fetchMerchant booking = CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)

-- | The merchant's own driver platform registers as "<host>/beckn/<driverOfferMerchantId>"; rides from
-- other providers (e.g. ONDC) have no during-ride feedback.
servedByOwnDriverPlatform :: DM.Merchant -> DB.Booking -> Bool
servedByOwnDriverPlatform merchant booking = merchant.driverOfferMerchantId `T.isSuffixOf` booking.providerId

resolveLanguage :: Id DP.Person -> Maybe Language -> Flow Language
resolveLanguage _ (Just language) = pure language
resolveLanguage personId Nothing = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  pure $ fromMaybe ENGLISH person.language
