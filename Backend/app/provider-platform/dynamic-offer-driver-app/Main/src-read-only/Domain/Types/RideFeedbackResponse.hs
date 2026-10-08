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
import qualified Tools.Beam.UtilsTH

data RideFeedbackResponse = RideFeedbackResponse
  { actionResults :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
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
    questionType :: Domain.Types.RideFeedbackConfig.RideFeedbackQuestionType,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    secondsSinceBooking :: Kernel.Prelude.Int,
    stage :: Domain.Types.RideFeedbackConfig.RideFeedbackStage,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    subjectId :: Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackActionResult = RideFeedbackActionResult
  { actionType :: Domain.Types.RideFeedbackConfig.RideFeedbackActionType,
    attempts :: Kernel.Prelude.Int,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    externalRef :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ruleId :: Kernel.Prelude.Text,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackActionStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackActionStatus = PENDING | SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

data RideFeedbackAnswer = RideFeedbackAnswer
  { mediaFileIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    number :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectedOptionKeys :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    text :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseStatus = SHOWN | ANSWERED | SKIPPED | EXPIRED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, (Enum))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackActionStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackResponseStatus))
