{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverBlockReason where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DriverBlockReasonT f
    = DriverBlockReasonT {blockReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                          blockTimeInHours :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                          reasonCode :: (B.C f Kernel.Prelude.Text),
                          merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                          merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverBlockReasonT
    where data PrimaryKey DriverBlockReasonT f = DriverBlockReasonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverBlockReasonId . reasonCode
type DriverBlockReason = DriverBlockReasonT Identity

$(enableKVPG (''DriverBlockReasonT) [('reasonCode)] [])

$(mkTableInstances (''DriverBlockReasonT) "driver_block_reason")

