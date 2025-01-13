{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyBooking where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data JourneyBooking = JourneyBooking
  { convenienceCost :: Kernel.Prelude.Int,
    customerCancelled :: Kernel.Prelude.Bool,
    estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.JourneyBooking.JourneyBooking,
    isBookingCancellable :: Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    modes :: [Domain.Types.Common.MultimodalTravelMode],
    numberOfPassengers :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
