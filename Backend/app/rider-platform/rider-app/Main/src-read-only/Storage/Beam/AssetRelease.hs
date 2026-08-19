{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AssetRelease where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.AssetRelease
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AssetReleaseT f = AssetReleaseT
  { assetType :: B.C f Domain.Types.Extra.AssetRelease.AssetType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    rolledBackAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    sha256 :: B.C f Kernel.Prelude.Text,
    sizeBytes :: B.C f Kernel.Prelude.Int,
    sourceRef :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    url :: B.C f Kernel.Prelude.Text,
    version :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table AssetReleaseT where
  data PrimaryKey AssetReleaseT f = AssetReleaseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AssetReleaseId . id

type AssetRelease = AssetReleaseT Identity

$(enableKVPG ''AssetReleaseT ['id] [['assetType]])

$(mkTableInstances ''AssetReleaseT "asset_release")
