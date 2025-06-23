{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultimodalPreferences where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MultimodalPreferences = MultimodalPreferences
  { allowedTransitModes :: [Domain.Types.Common.MultimodalTravelMode],
    busTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    journeyOptionsSortingType :: Domain.Types.MultimodalPreferences.JourneyOptionsSortingType,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    subwayTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data JourneyOptionsSortingType = FASTEST | CHEAPEST | MINIMUM_TRANSITS | MOST_RELEVANT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''JourneyOptionsSortingType)
