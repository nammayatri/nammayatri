{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSStageFare where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FRFSStageFare
    = FRFSStageFare {amount :: Kernel.Types.Common.HighPrecMoney,
                     currency :: Kernel.Types.Common.Currency,
                     farePolicyId :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
                     merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                     stage :: Kernel.Prelude.Int,
                     createdAt :: Kernel.Prelude.UTCTime,
                     updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



