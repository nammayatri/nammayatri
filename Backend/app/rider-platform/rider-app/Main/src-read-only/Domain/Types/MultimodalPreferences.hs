{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MultimodalPreferences where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.Common
import qualified BecknV2.FRFS.Enums
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data MultimodalPreferences
    = MultimodalPreferences {allowedTransitModes :: [Domain.Types.Common.MultimodalTravelMode],
                             busTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
                             journeyOptionsSortingType :: Domain.Types.MultimodalPreferences.JourneyOptionsSortingType,
                             personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                             subwayTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
                             merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                             merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                             createdAt :: Kernel.Prelude.UTCTime,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show))
data JourneyOptionsSortingType = FASTEST | CHEAPEST | MINIMUM_TRANSITS | MOST_RELEVANT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''JourneyOptionsSortingType))

