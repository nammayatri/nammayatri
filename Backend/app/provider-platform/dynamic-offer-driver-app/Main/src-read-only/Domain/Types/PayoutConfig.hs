{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PayoutConfig = PayoutConfig
  { batchLimit :: Kernel.Prelude.Int,
    isPayoutEnabled :: Kernel.Prelude.Bool,
    maxRetryCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    payoutRegistrationAmount :: Kernel.Types.Common.HighPrecMoney,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    thresholdPayoutAmountPerPerson :: Kernel.Types.Common.HighPrecMoney,
    timeDiff :: Kernel.Prelude.NominalDiffTime,
    vehicleVariant :: Domain.Types.Vehicle.Variant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
