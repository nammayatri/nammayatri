{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CancellationReason where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Domain.Types.CancellationReason
import qualified Database.Beam as B



data CancellationReasonT f
    = CancellationReasonT {description :: (B.C f Data.Text.Text),
                           enabled :: (B.C f Kernel.Prelude.Bool),
                           priority :: (B.C f Kernel.Prelude.Int),
                           reasonCode :: (B.C f Domain.Types.CancellationReason.CancellationReasonCode),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CancellationReasonT
    where data PrimaryKey CancellationReasonT f = CancellationReasonId (B.C f Domain.Types.CancellationReason.CancellationReasonCode) deriving (Generic, B.Beamable)
          primaryKey = CancellationReasonId . reasonCode
type CancellationReason = CancellationReasonT Identity

$(enableKVPG (''CancellationReasonT) [('reasonCode)] [])

$(mkTableInstances (''CancellationReasonT) "cancellation_reason")

