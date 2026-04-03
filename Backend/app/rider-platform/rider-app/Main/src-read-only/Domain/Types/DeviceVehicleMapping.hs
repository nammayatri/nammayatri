{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DeviceVehicleMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Time
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data DeviceVehicleMapping
    = DeviceVehicleMapping {createdAt :: Data.Time.UTCTime,
                            deviceId :: Data.Text.Text,
                            gtfsId :: Data.Text.Text,
                            updatedAt :: Data.Time.UTCTime,
                            vehicleNo :: Data.Text.Text,
                            merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                            merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



