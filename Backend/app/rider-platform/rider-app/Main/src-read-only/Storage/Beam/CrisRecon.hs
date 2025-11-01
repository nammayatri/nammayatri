{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CrisRecon where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data CrisReconT f = CrisReconT
  { bppOrderId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    dateIst :: (B.C f Kernel.Prelude.Text),
    fareAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table CrisReconT where
  data PrimaryKey CrisReconT f = CrisReconId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CrisReconId . id

type CrisRecon = CrisReconT Identity

$(enableKVPG (''CrisReconT) [('id)] [])

$(mkTableInstances (''CrisReconT) "cris_recon")
