{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RideFeedbackResponse where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.RideFeedbackConfig
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Feedback.Types
import qualified Tools.Beam.UtilsTH

data RideFeedbackResponse = RideFeedbackResponse
  { actionResults :: Kernel.Prelude.Maybe [Lib.Feedback.Types.FeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Lib.Feedback.Types.FeedbackAnswer,
    answerScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    configPilotVersions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    questionKey :: Kernel.Prelude.Text,
    questionType :: Lib.Feedback.Types.FeedbackQuestionType,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    secondsSinceBooking :: Kernel.Prelude.Int,
    stage :: Domain.Types.RideFeedbackConfig.RideFeedbackStage,
    status :: Lib.Feedback.Types.FeedbackResponseStatus,
    subjectId :: Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
