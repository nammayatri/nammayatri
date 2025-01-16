{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BapMetadata where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BapMetadataT f = BapMetadataT
  { domain :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    id :: B.C f Data.Text.Text,
    logoUrl :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    name :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BapMetadataT where
  data PrimaryKey BapMetadataT f = BapMetadataId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = BapMetadataId . id

type BapMetadata = BapMetadataT Identity

$(enableKVPG ''BapMetadataT ['id] [])

$(mkTableInstances ''BapMetadataT "bap_metadata")
