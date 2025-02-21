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
    expand :: Kernel.Prelude.Maybe Kernel.External.Payout.Interface.Types.Expand,
    id :: Kernel.Types.Id.Id Domain.Types.PayoutConfig.PayoutConfig,
    isPayoutEnabled :: Kernel.Prelude.Bool,
    maxPayoutReferralForADay :: Kernel.Prelude.Int,
    maxRetryCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    orderType :: Kernel.Prelude.Text,
    payoutEntity :: Domain.Types.PayoutConfig.PayoutEntity,
    referralRewardAmountPerRide :: Kernel.Types.Common.HighPrecMoney,
    referredByRewardAmount :: Kernel.Types.Common.HighPrecMoney,
    remark :: Kernel.Prelude.Text,
    thresholdPayoutAmountPerPerson :: Kernel.Types.Common.HighPrecMoney,
    timeDiff :: Kernel.Prelude.NominalDiffTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

data PayoutEntity = METRO_TICKET_CASHBACK | PAYOUT_AWARD deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutEntity)
