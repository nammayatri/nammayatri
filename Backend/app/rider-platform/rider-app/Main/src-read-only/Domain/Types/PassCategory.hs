{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PassCategory where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data PassCategory
    = PassCategory {description :: Kernel.Prelude.Text,
                    id :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
                    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                    name :: Kernel.Prelude.Text,
                    createdAt :: Kernel.Prelude.UTCTime,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



