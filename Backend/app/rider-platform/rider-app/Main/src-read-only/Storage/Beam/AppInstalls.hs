{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.AppInstalls where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data AppInstallsT f
    = AppInstallsT {appVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    bundleVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    createdAt :: (B.C f Kernel.Prelude.UTCTime),
                    deviceToken :: (B.C f Kernel.Prelude.Text),
                    id :: (B.C f Kernel.Prelude.Text),
                    merchantId :: (B.C f Kernel.Prelude.Text),
                    platform :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                    source :: (B.C f Kernel.Prelude.Text),
                    updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table AppInstallsT
    where data PrimaryKey AppInstallsT f = AppInstallsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = AppInstallsId . id
type AppInstalls = AppInstallsT Identity

$(enableKVPG (''AppInstallsT) [('id)] [])

$(mkTableInstances (''AppInstallsT) "app_installs")

