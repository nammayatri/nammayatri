{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RefundRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.Extra.RefundRequest
import qualified Kernel.Utils.Common
import qualified Kernel.Prelude
import qualified Domain.Types.RefundRequest
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data RefundRequestT f
    = RefundRequestT {code :: (B.C f Domain.Types.Extra.RefundRequest.RefundRequestCode),
                      currency :: (B.C f Kernel.Utils.Common.Currency),
                      description :: (B.C f Kernel.Prelude.Text),
                      evidenceS3Path :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      id :: (B.C f Kernel.Prelude.Text),
                      merchantId :: (B.C f Kernel.Prelude.Text),
                      merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                      orderId :: (B.C f Kernel.Prelude.Text),
                      personId :: (B.C f Kernel.Prelude.Text),
                      refundPurpose :: (B.C f Domain.Types.RefundRequest.RefundPurpose),
                      refundsAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                      refundsId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      refundsTries :: (B.C f Kernel.Prelude.Int),
                      requestedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                      responseDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      status :: (B.C f Domain.Types.RefundRequest.RefundRequestStatus),
                      transactionAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                      transactionId :: (B.C f Kernel.Prelude.Text),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RefundRequestT
    where data PrimaryKey RefundRequestT f = RefundRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RefundRequestId . id
type RefundRequest = RefundRequestT Identity

$(enableKVPG (''RefundRequestT) [('id)] [[('orderId)]])

$(mkTableInstances (''RefundRequestT) "refund_request")

