{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSRouteStopStageFare where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FRFSRouteStopStageFare
    = FRFSRouteStopStageFare {farePolicyId :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
                              merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                              merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                              routeCode :: Kernel.Prelude.Text,
                              stage :: Kernel.Prelude.Int,
                              stopCode :: Kernel.Prelude.Text,
                              createdAt :: Kernel.Prelude.UTCTime,
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



