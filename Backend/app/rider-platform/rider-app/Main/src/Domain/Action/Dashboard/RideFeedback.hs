module Domain.Action.Dashboard.RideFeedback
  ( postRideFeedbackBookingResponseRetryActions,
  )
where

import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.RideFeedback.Actions (ActionEnv (..), rideSubjectLines, runActionsAndReport)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-- | Re-runs an answer's actions that have not succeeded yet (e.g. after Kapture or the driver platform
-- was down). The driver platform says which ones; actions that already succeeded are never repeated.
-- bookingId and responseId are the driver platform's, as the dashboard's inspector shows them.
postRideFeedbackBookingResponseRetryActions :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Flow APISuccess
postRideFeedbackBookingResponseRetryActions merchantShortId opCity bppBookingId responseId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  moc <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  booking <- QB.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingNotFound bppBookingId)
  unless (booking.merchantOperatingCityId == moc.id) $ throwError (InvalidRequest "Booking does not belong to this merchant city")
  retryable <- CallBPPInternal.rideFeedbackRetryableActions merchant.driverOfferApiKey merchant.driverOfferBaseUrl bppBookingId responseId
  when (null retryable.actions) $ throwError (InvalidRequest "No failed or pending actions to retry")
  ride <- maybe (pure Nothing) (QRide.findByBPPRideId . Id) retryable.rideId
  person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  riderConfig <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  runActionsAndReport
    ActionEnv {merchant, riderConfig, person, ride, booking = Just booking, subjectLines = rideSubjectLines booking ride, questionKey = retryable.questionKey, answer = retryable.answer}
    bppBookingId
    retryable.responseId
    retryable.actions
  pure Success
