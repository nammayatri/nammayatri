{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSRouteFareProduct where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Types.TimeBound
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified BecknV2.FRFS.Enums
import qualified Tools.Beam.UtilsTH



data FRFSRouteFareProduct
    = FRFSRouteFareProduct {farePolicyId :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
                            id :: Kernel.Types.Id.Id Domain.Types.FRFSRouteFareProduct.FRFSRouteFareProduct,
                            integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                            merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                            merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                            routeCode :: Kernel.Prelude.Text,
                            timeBounds :: Kernel.Types.TimeBound.TimeBound,
                            vehicleServiceTierId :: Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier,
                            vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
                            createdAt :: Kernel.Prelude.UTCTime,
                            updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



