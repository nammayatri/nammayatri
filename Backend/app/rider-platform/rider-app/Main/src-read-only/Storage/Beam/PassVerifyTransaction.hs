{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PassVerifyTransaction where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PassVerifyTransactionT f
    = PassVerifyTransactionT {autoActivated :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              destinationStopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              fleetId :: (B.C f Kernel.Prelude.Text),
                              id :: (B.C f Kernel.Prelude.Text),
                              isActuallyValid :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              purchasePassId :: (B.C f Kernel.Prelude.Text),
                              sourceStopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              validTill :: (B.C f Kernel.Prelude.UTCTime),
                              verifiedAt :: (B.C f Kernel.Prelude.UTCTime),
                              merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PassVerifyTransactionT
    where data PrimaryKey PassVerifyTransactionT f = PassVerifyTransactionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PassVerifyTransactionId . id
type PassVerifyTransaction = PassVerifyTransactionT Identity

$(enableKVPG (''PassVerifyTransactionT) [('id)] [[('purchasePassId)]])

$(mkTableInstances (''PassVerifyTransactionT) "pass_verify_transaction")

