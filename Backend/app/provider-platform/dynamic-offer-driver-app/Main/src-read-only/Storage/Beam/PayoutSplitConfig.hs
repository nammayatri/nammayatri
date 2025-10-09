{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutSplitConfig where

import qualified Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data PayoutSplitConfigT f = PayoutSplitConfigT
  { area :: (B.C f (Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area)),
    bankDetails :: (B.C f Data.Aeson.Value),
    id :: (B.C f Data.Text.Text),
    vendorId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutSplitConfigT where
  data PrimaryKey PayoutSplitConfigT f = PayoutSplitConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutSplitConfigId . id

type PayoutSplitConfig = PayoutSplitConfigT Identity

$(enableKVPG (''PayoutSplitConfigT) [('id)] [])

$(mkTableInstances (''PayoutSplitConfigT) "payout_split_config")
