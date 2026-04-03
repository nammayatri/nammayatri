{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.VendorSplitDetails where
import Kernel.Prelude
import Data.Aeson
import qualified Lib.Types.SpecialLocation
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleVariant
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data VendorSplitDetails
    = VendorSplitDetails {area :: Lib.Types.SpecialLocation.Area,
                          maxVendorFeeAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                          splitType :: Domain.Types.VendorSplitDetails.SplitType,
                          splitValue :: Kernel.Prelude.Double,
                          vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
                          vendorId :: Data.Text.Text,
                          createdAt :: Kernel.Prelude.UTCTime,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data SplitType = FIXED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SplitType))

