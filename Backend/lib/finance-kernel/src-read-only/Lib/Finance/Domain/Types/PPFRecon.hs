{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.PPFRecon where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH

data PPFRecon = PPFRecon
  { buyerAppCommission :: Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount,
    collectorSubscriberId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Prelude.Text,
    domain :: Lib.Finance.Domain.Types.PPFRecon.PPFDomain,
    gstAmount :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PPFRecon.PPFRecon,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    networkFee :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount,
    networkOrderId :: Kernel.Prelude.Text,
    orderAmount :: Lib.Finance.Domain.Types.PPFRecon.ReconSettlementAmount,
    receiverSubscriberId :: Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Text,
    settlementStatus :: Lib.Finance.Domain.Types.PPFRecon.PPFSettlementStatus,
    transactionId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

data PPFDomain = MOBILITY | FRFS deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

data PPFSettlementStatus = SETTLED | PENDING | TO_BE_INITIATED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (Kernel.Prelude.ToParamSchema))

data ReconSettlementAmount = ReconSettlementAmount {expected :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney, settled :: Kernel.Types.Common.HighPrecMoney}
  deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFDomain))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFDomain))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFSettlementStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFSettlementStatus))
