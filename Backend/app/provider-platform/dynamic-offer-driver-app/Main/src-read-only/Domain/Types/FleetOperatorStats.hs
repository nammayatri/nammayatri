{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FleetOperatorStats where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FleetOperatorStats
    = FleetOperatorStats {acceptationRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
                          customerCancellationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          distanceUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit,
                          driverCancellationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          driverFirstSubscription :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          fleetOperatorId :: Kernel.Prelude.Text,
                          inspectionCompleted :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          totalCompletedRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
                          totalEarning :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                          totalRatingCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          totalRatingScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          totalRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                          merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                          merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                          createdAt :: Kernel.Prelude.UTCTime,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



