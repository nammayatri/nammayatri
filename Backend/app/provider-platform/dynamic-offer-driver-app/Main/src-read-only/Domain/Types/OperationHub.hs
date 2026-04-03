{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.OperationHub where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data OperationHub
    = OperationHub {address :: Kernel.Prelude.Text,
                    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                    id :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
                    lat :: Kernel.Prelude.Double,
                    lon :: Kernel.Prelude.Double,
                    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                    mobileNumber :: Kernel.Prelude.Text,
                    name :: Kernel.Prelude.Text,
                    createdAt :: Kernel.Prelude.UTCTime,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



