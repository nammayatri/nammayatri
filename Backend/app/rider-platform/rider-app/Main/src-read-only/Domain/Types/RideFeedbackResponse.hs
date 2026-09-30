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
import qualified Domain.Types.RideStatus
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RideFeedbackResponse = RideFeedbackResponse
  { actionResults :: Kernel.Prelude.Maybe [Domain.Types.RideFeedbackResponse.RideFeedbackActionResult],
    answer :: Kernel.Prelude.Maybe Domain.Types.RideFeedbackResponse.RideFeedbackAnswer,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    configId :: Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig,
    configVersion :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    logicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    parentResponseId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse),
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    questionKey :: Kernel.Prelude.Text,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    rideStatusAtResponse :: Kernel.Prelude.Maybe Domain.Types.RideStatus.RideStatus,
    secondsIntoRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectedOptionKeys :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    shownCount :: Kernel.Prelude.Int,
    status :: Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTierType :: Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType,
    vehicleVariant :: Kernel.Prelude.Maybe Kernel.Prelude.Text
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

data RideFeedbackActionStatus = PENDING | SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideFeedbackAnswer = RideFeedbackAnswer
  { mediaFileIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    number :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectedOptionKeys :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    text :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RideFeedbackResponseStatus = SHOWN | ANSWERED | SKIPPED | DISMISSED | EXPIRED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackActionStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RideFeedbackResponseStatus))
