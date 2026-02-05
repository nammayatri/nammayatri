{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.StopFare where

import Data.Aeson
import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data StopFare = StopFare
  { amount :: Kernel.Types.Common.HighPrecMoney,
    bppItemId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    currency :: Kernel.Types.Common.Currency,
    endStopCode :: Kernel.Prelude.Text,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    offeredAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    startStopCode :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
