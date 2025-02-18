{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleConfig where

import Data.Aeson
import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleConfig = VehicleConfig
  { becknConfigId :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    blackListedSubscribers :: [Kernel.Prelude.Text],
    buyerFinderFee :: Kernel.Prelude.Text,
    category :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleConfig.VehicleConfig,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
