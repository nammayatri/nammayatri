{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Mandate where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.Mandate
import qualified Database.Beam as B



data MandateT f
    = MandateT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                endDate :: (B.C f Kernel.Prelude.UTCTime),
                id :: (B.C f Kernel.Prelude.Text),
                mandatePaymentFlow :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                maxAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                payerApp :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                payerAppName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                payerVpa :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                startDate :: (B.C f Kernel.Prelude.UTCTime),
                status :: (B.C f Domain.Types.Mandate.MandateStatus),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table MandateT
    where data PrimaryKey MandateT f = MandateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = MandateId . id
type Mandate = MandateT Identity

$(enableKVPG (''MandateT) [('id)] [])

$(mkTableInstances (''MandateT) "mandate")

