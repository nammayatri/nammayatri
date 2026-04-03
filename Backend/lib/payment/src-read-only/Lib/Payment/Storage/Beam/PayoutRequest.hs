{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.PayoutRequest where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutRequest
import qualified Database.Beam as B



data PayoutRequestT f
    = PayoutRequestT {amount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                      beneficiaryId :: (B.C f Kernel.Prelude.Text),
                      cashMarkedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      cashMarkedById :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      cashMarkedByName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      city :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      coverageFrom :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      coverageTo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      customerEmail :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      customerName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      customerPhone :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      customerVpa :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      entityId :: (B.C f Kernel.Prelude.Text),
                      entityName :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName)),
                      entityRefId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      expectedCreditTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      id :: (B.C f Kernel.Prelude.Text),
                      merchantId :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      orderType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      payoutFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                      payoutTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      payoutType :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.PayoutRequest.PayoutType)),
                      remark :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      retryCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      scheduledAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                      status :: (B.C f Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PayoutRequestT
    where data PrimaryKey PayoutRequestT f = PayoutRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PayoutRequestId . id
type PayoutRequest = PayoutRequestT Identity

$(enableKVPG (''PayoutRequestT) [('id)] [])

$(mkTableInstancesGenericSchema (''PayoutRequestT) "payout_request")

