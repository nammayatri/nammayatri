{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BppDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BppDetailsT f = BppDetailsT
  { description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    domain :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    logoUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    name :: B.C f Kernel.Prelude.Text,
    subscriberId :: B.C f Kernel.Prelude.Text,
    supportNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BppDetailsT where
  data PrimaryKey BppDetailsT f = BppDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BppDetailsId . id

type BppDetails = BppDetailsT Identity

$(enableKVPG ''BppDetailsT ['id] [['subscriberId]])

$(mkTableInstances ''BppDetailsT "bpp_details")
