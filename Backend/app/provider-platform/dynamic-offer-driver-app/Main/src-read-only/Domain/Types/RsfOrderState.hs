{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RsfOrderState where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RsfOrderState = RsfOrderState
  { bapId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    lastClaimedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastReportedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastReportedMessageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    orderId :: Kernel.Prelude.Text,
    reportedCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reportedDiff :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    reportedStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
