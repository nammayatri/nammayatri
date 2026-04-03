{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RoutePolylines where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified BecknV2.FRFS.Enums
import qualified Tools.Beam.UtilsTH



data RoutePolylines
    = RoutePolylines {createdAt :: Kernel.Prelude.UTCTime,
                      id :: Kernel.Types.Id.Id Domain.Types.RoutePolylines.RoutePolylines,
                      merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                      polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      routeId :: Kernel.Prelude.Text,
                      updatedAt :: Kernel.Prelude.UTCTime,
                      vehicleType :: BecknV2.FRFS.Enums.VehicleCategory}
    deriving (Show, ( Generic), ( ToJSON), ( FromJSON), ( Eq))



