{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RCValidationRules where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data RCValidationRules
    = RCValidationRules {fuelType :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
                         id :: Kernel.Types.Id.Id Domain.Types.RCValidationRules.RCValidationRules,
                         maxVehicleAge :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                         vehicleClass :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
                         vehicleOEM :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



