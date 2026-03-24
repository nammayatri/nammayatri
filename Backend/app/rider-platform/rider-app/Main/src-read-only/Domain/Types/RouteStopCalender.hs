{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RouteStopCalender where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data RouteStopCalender
    = RouteStopCalender {integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                         serviceability :: [Kernel.Prelude.Int],
                         tripId :: Kernel.Types.Id.Id Domain.Types.RouteStopTimeTable.RouteStopTimeTable,
                         merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                         merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



