{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.FinanceTdsReimbursementRequest where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import Tools.Beam.UtilsTH

data FinanceTdsReimbursementRequestT f = FinanceTdsReimbursementRequestT
  { assessmentYear :: (B.C f Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.AssessmentYear),
    certAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    certNumber :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    documentId :: (B.C f Kernel.Prelude.Text),
    fleetOwnerId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quarter :: (B.C f Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.Quarter),
    rejectionReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequestStatus),
    tanNumber :: (B.C f Kernel.Prelude.Text),
    tdsRate :: (B.C f Kernel.Prelude.Double),
    tdsSection :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FinanceTdsReimbursementRequestT where
  data PrimaryKey FinanceTdsReimbursementRequestT f = FinanceTdsReimbursementRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FinanceTdsReimbursementRequestId . id

type FinanceTdsReimbursementRequest = FinanceTdsReimbursementRequestT Identity

$(enableKVPG (''FinanceTdsReimbursementRequestT) [('id)] [[('fleetOwnerId)]])

$(mkTableInstancesGenericSchema (''FinanceTdsReimbursementRequestT) "finance_tds_reimbursement_request")
