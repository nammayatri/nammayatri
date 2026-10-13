{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DocumentVersionMetadata where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DocumentVersionMetadataT f = DocumentVersionMetadataT
  { documentNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    documentNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    documentType :: (B.C f Domain.Types.DocumentVerificationConfig.DocumentType),
    documentVersionId :: (B.C f Kernel.Prelude.Text),
    metadata :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    personId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DocumentVersionMetadataT where
  data PrimaryKey DocumentVersionMetadataT f = DocumentVersionMetadataId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DocumentVersionMetadataId . documentVersionId

type DocumentVersionMetadata = DocumentVersionMetadataT Identity

$(enableKVPG (''DocumentVersionMetadataT) [('documentVersionId)] [[('personId)]])

$(mkTableInstances (''DocumentVersionMetadataT) "document_version_metadata")
