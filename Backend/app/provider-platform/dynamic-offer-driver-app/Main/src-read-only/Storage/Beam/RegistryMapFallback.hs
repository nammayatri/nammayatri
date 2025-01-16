{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RegistryMapFallback where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RegistryMapFallbackT f = RegistryMapFallbackT {registryUrl :: B.C f Kernel.Prelude.Text, subscriberId :: B.C f Kernel.Prelude.Text, uniqueId :: B.C f Kernel.Prelude.Text}
  deriving (Generic, B.Beamable)

instance B.Table RegistryMapFallbackT where
  data PrimaryKey RegistryMapFallbackT f = RegistryMapFallbackId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RegistryMapFallbackId <$> subscriberId <*> uniqueId

type RegistryMapFallback = RegistryMapFallbackT Identity

$(enableKVPG ''RegistryMapFallbackT ['subscriberId, 'uniqueId] [])

$(mkTableInstances ''RegistryMapFallbackT "registry_map_fallback")
