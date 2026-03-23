{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Kernel.External.Payout.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PayoutConfig = PayoutConfig
  { batchLimit :: Kernel.Prelude.Int,
    coinRedemptionMinimumLimit :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    d2dPayoutType :: Domain.Types.PayoutConfig.D2DPayoutTypeEnum,
    expand :: Kernel.Prelude.Maybe Kernel.External.Payout.Interface.Types.Expand,
    isPayoutEnabled :: Kernel.Prelude.Bool,
    maxPayoutReferralForADay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxRetryCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    orderType :: Kernel.Prelude.Text,
    payoutRegistrationCgst :: Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationFee :: Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationSgst :: Kernel.Types.Common.HighPrecMoney,
    referralProgramStartDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    referralRewardAmountPerRideForD2DPayout :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    remark :: Kernel.Prelude.Text,
    thresholdPayoutAmountPerPerson :: Kernel.Types.Common.HighPrecMoney,
    timeDiff :: Kernel.Prelude.NominalDiffTime,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    vpaVerificationMode :: Domain.Types.PayoutConfig.VpaVerificationMode,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data D2DPayoutTypeEnum = WALLET | DIRECT_PAYOUT | NO_PAYOUT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VpaVerificationMode = PAYMENT_BASED | API_BASED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''D2DPayoutTypeEnum)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VpaVerificationMode)
