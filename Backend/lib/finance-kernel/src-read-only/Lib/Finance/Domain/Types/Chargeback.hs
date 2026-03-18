{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Chargeback where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport

data Chargeback = Chargeback
  { id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Chargeback.Chargeback,
    settlementReportId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport,
    transactionId :: Kernel.Prelude.Text,
    chargebackReasonCode :: Kernel.Prelude.Text,
    chargebackAmount :: Kernel.Types.Common.HighPrecMoney,
    chargebackStatus :: Lib.Finance.Domain.Types.Chargeback.ChargebackStatus,
    responseDeadline :: Kernel.Prelude.UTCTime,
    evidenceUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminNotes :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data ChargebackStatus = OPEN | EVIDENCE_SUBMITTED | WON | LOST | EXPIRED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ChargebackStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''ChargebackStatus))
