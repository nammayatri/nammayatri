{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EndOtpConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EndOtpConfigT f = EndOtpConfigT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    isEndOtpRequired :: (B.C f Kernel.Prelude.Bool),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    tripCategory :: (B.C f Kernel.Prelude.Text),
    tripMode :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table EndOtpConfigT where
  data PrimaryKey EndOtpConfigT f = EndOtpConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EndOtpConfigId <$> merchantOperatingCityId <*> tripCategory <*> tripMode

type EndOtpConfig = EndOtpConfigT Identity

$(enableKVPG (''EndOtpConfigT) [('merchantOperatingCityId), ('tripCategory), ('tripMode)] [])

$(mkTableInstances (''EndOtpConfigT) "end_otp_config")
