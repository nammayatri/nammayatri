{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PayoutStatusHistory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ScheduledPayout
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PayoutStatusHistory = PayoutStatusHistory
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PayoutStatusHistory.PayoutStatusHistory,
    message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scheduledPayoutId :: Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout,
    status :: Domain.Types.ScheduledPayout.ScheduledPayoutStatus,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
