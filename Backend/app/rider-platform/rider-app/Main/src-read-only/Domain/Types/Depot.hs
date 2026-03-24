{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Depot where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data Depot
    = Depot {createdAt :: Kernel.Prelude.UTCTime,
             id :: Kernel.Types.Id.Id Domain.Types.Depot.Depot,
             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
             merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
             name :: Kernel.Prelude.Text,
             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



