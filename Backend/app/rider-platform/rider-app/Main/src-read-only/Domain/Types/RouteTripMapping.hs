{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RouteTripMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified BecknV2.FRFS.Enums
import qualified Tools.Beam.UtilsTH



data RouteTripMapping
    = RouteTripMapping {createdAt :: Kernel.Prelude.UTCTime,
                        integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                        merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                        merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                        routeCode :: Kernel.Prelude.Text,
                        tripCode :: Kernel.Prelude.Text,
                        tripEndTime :: Kernel.Prelude.Text,
                        tripStartTime :: Kernel.Prelude.Text,
                        updatedAt :: Kernel.Prelude.UTCTime,
                        vehicleType :: BecknV2.FRFS.Enums.VehicleCategory}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



