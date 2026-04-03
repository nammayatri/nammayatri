{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.VehicleSeatLayoutMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SeatLayout
import qualified Tools.Beam.UtilsTH



data VehicleSeatLayoutMapping
    = VehicleSeatLayoutMapping {gtfsId :: Kernel.Prelude.Text,
                                id :: Kernel.Types.Id.Id Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping,
                                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                                seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout,
                                vehicleNo :: Kernel.Prelude.Text,
                                createdAt :: Kernel.Prelude.UTCTime,
                                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Show, ( Generic), ( ToJSON), ( FromJSON), ( Eq), ( ToSchema))



