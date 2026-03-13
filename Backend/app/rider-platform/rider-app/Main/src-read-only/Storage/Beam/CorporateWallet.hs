{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateWallet where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateWalletT f = CorporateWalletT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    balance :: B.C f Kernel.Prelude.Double,
    currency :: B.C f Kernel.Prelude.Text,
    status :: B.C f Kernel.Prelude.Text,
    graceStartedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastTopUpAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateWalletT where
  data PrimaryKey CorporateWalletT f = CorporateWalletId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateWalletId . id

type CorporateWallet = CorporateWalletT Identity

$(enableKVPG ''CorporateWalletT ['id] [['corporateEntityId]])

$(mkTableInstances ''CorporateWalletT "corporate_wallet")
