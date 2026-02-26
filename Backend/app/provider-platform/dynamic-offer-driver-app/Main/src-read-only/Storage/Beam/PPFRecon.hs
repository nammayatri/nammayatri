{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PPFRecon where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.PPFRecon
import qualified Database.Beam as B



data PPFReconT f
    = PPFReconT {beneficiaryBankAccount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 beneficiaryIFSC :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 buyerAppCommission :: (B.C f Kernel.Types.Common.HighPrecMoney),
                 collectorBankAccount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 collectorIFSC :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 collectorSubscriberId :: (B.C f Kernel.Prelude.Text),
                 differenceAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 domain :: (B.C f Domain.Types.PPFRecon.PPFDomain),
                 entityId :: (B.C f Kernel.Prelude.Text),
                 entityType :: (B.C f Domain.Types.PPFRecon.PPFEntityType),
                 fulfilledTimestamp :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                 gstAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 id :: (B.C f Kernel.Prelude.Text),
                 merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                 merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                 message :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 networkFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 networkOrderId :: (B.C f Kernel.Prelude.Text),
                 currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                 orderAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                 orderStatus :: (B.C f Kernel.Prelude.Text),
                 paymentAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                 paymentReference :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 paymentStatus :: (B.C f Domain.Types.PPFRecon.PPFPaymentStatus),
                 paymentTransactionId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 receiverSubscriberId :: (B.C f Kernel.Prelude.Text),
                 reconCompletedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                 reconInitiatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                 sellerShare :: (B.C f Kernel.Types.Common.HighPrecMoney),
                 settledTimestamp :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                 settlementAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                 settlementRefNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                 settlementStatus :: (B.C f Domain.Types.PPFRecon.PPFSettlementStatus),
                 tcs :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 tds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 transactionId :: (B.C f Kernel.Prelude.Text),
                 withholdingAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                 createdAt :: (B.C f Kernel.Prelude.UTCTime),
                 updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PPFReconT
    where data PrimaryKey PPFReconT f = PPFReconId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PPFReconId . id
type PPFRecon = PPFReconT Identity

$(enableKVPG (''PPFReconT) [('id)] [])

$(mkTableInstances (''PPFReconT) "ppf_recon")

