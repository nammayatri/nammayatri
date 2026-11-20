{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AppAttestation.Storage.Beam.AppAttestKey where

import qualified AppAttestation.Domain.Types.Common
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data AppAttestKeyT f = AppAttestKeyT
  { bundleId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    environment :: (B.C f AppAttestation.Domain.Types.Common.AppAttestEnvironment),
    keyId :: (B.C f Kernel.Prelude.Text),
    lastUsedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    publicKey :: (B.C f Kernel.Prelude.Text),
    receipt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    signCount :: (B.C f Kernel.Prelude.Int),
    status :: (B.C f AppAttestation.Domain.Types.Common.AppAttestKeyStatus),
    teamId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    useCount :: (B.C f Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppAttestKeyT where
  data PrimaryKey AppAttestKeyT f = AppAttestKeyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AppAttestKeyId . keyId

type AppAttestKey = AppAttestKeyT Identity

$(enableKVPG (''AppAttestKeyT) [('keyId)] [])

$(mkTableInstancesGenericSchema (''AppAttestKeyT) "app_attest_key")
