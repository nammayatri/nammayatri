{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Journey where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Tools.Beam.UtilsTH

data Journey = Journey
  { convenienceCost :: Kernel.Prelude.Int,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    fromLocation :: Domain.Types.Location.Location,
    hasPreferredServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasPreferredTransitModes :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasStartedTrackingWithoutBooking :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    isPaymentSuccess :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPublicTransportIncluded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSingleMode :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    modes :: [Domain.Types.Common.MultimodalTravelMode],
    paymentOrderShortId :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    relevanceScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    searchRequestId :: Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.Journey.JourneyStatus,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    totalLegs :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data JourneyStatus
  = NEW
  | INITIATED
  | CONFIRMED
  | INPROGRESS
  | CANCELLED
  | FEEDBACK_PENDING
  | COMPLETED
  | FAILED
  | EXPIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''JourneyStatus)
