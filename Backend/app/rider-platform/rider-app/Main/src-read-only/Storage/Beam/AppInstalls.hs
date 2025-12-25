{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AppInstalls where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AppInstallsT f = AppInstallsT
  { appVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    deviceToken :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    platform :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    source :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AppInstallsT where
  data PrimaryKey AppInstallsT f = AppInstallsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AppInstallsId . id

type AppInstalls = AppInstallsT Identity

$(enableKVPG ''AppInstallsT ['id] [])

$(mkTableInstances ''AppInstallsT "app_installs")
