{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyLegsFeedbacks where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data JourneyLegsFeedbacks = JourneyLegsFeedbacks
  { isExperienceGood :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legOrder :: Kernel.Prelude.Int,
    travelMode :: Kernel.Prelude.Maybe Domain.Types.Common.MultimodalTravelMode,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
