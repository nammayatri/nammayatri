{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconUtrSettlement where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ReconUtrSettlement = ReconUtrSettlement
  { bankVerifiedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Prelude.Text,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    resolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    resolvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalOrders :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
