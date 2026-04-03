{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSVehicleServiceTier where
import Kernel.Prelude
import Data.Aeson
import qualified BecknV2.FRFS.Enums
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FRFSVehicleServiceTier
    = FRFSVehicleServiceTier {_type :: BecknV2.FRFS.Enums.ServiceTierType,
                              description :: Kernel.Prelude.Text,
                              id :: Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier,
                              integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                              isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                              longName :: Kernel.Prelude.Text,
                              merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                              merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                              providerCode :: Kernel.Prelude.Text,
                              shortName :: Kernel.Prelude.Text,
                              trainType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                              createdAt :: Kernel.Prelude.UTCTime,
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



