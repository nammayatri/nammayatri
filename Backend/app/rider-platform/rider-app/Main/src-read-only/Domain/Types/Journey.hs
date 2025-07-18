{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Journey where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.SearchRequest
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
    fromLocationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hasPreferredServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    hasPreferredTransitModes :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    isPaymentSuccess :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isPublicTransportIncluded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    modes :: [Domain.Types.Common.MultimodalTravelMode],
    paymentOrderShortId :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    relevanceScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    searchRequestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.Journey.JourneyStatus,
    toLocationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalLegs :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data JourneyStatus
  = NEW
  | INITIATED
  | CONFIRMED
  | INPROGRESS
  | FEEDBACK_PENDING
  | CANCELLED
  | COMPLETED
  | FAILED
  | EXPIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''JourneyStatus)
