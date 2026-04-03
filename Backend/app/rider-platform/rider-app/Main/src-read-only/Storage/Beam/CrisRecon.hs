{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CrisRecon where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data CrisReconT f
    = CrisReconT {bppOrderId :: (B.C f Kernel.Prelude.Text),
                  createdAt :: (B.C f Kernel.Prelude.UTCTime),
                  dateIst :: (B.C f Kernel.Prelude.Text),
                  fareAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                  id :: (B.C f Kernel.Prelude.Text),
                  updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                  merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                  merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table CrisReconT
    where data PrimaryKey CrisReconT f = CrisReconId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CrisReconId . id
type CrisRecon = CrisReconT Identity

$(enableKVPG (''CrisReconT) [('id)] [])

$(mkTableInstances (''CrisReconT) "cris_recon")

