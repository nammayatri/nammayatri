{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ReconUtrSettlement where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ReconUtrSettlement = ReconUtrSettlement
  { approvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    approvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankVerifiedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bapId :: Kernel.Prelude.Text,
    bapUri :: Kernel.Prelude.Text,
    claimedTotalAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    deadline :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconUtrSettlement.ReconUtrSettlement,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    resolutionStatus :: Lib.Finance.Domain.Types.ReconUtrSettlement.UtrResolutionStatus,
    sendAttempts :: Kernel.Prelude.Int,
    sendStatus :: Lib.Finance.Domain.Types.ReconUtrSettlement.UtrSendStatus,
    sentAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalOrders :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UtrResolutionStatus = RES_PENDING | APPROVED | REVISED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data UtrSendStatus = SEND_PENDING | SENT | SEND_FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''UtrResolutionStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''UtrSendStatus))
