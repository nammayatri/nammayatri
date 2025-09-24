{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MediaFileDocument where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.MediaFileDocument
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MediaFileDocumentT f = MediaFileDocumentT
  { creatorId :: (B.C f Kernel.Prelude.Text),
    fileHash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    mediaFileDocumentType :: (B.C f Domain.Types.Common.MediaFileDocumentType),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    rcId :: (B.C f Kernel.Prelude.Text),
    s3Path :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.MediaFileDocument.MediaFileDocumentStatus),
    uploadLink :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MediaFileDocumentT where
  data PrimaryKey MediaFileDocumentT f = MediaFileDocumentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MediaFileDocumentId . id

type MediaFileDocument = MediaFileDocumentT Identity

$(enableKVPG (''MediaFileDocumentT) [('id)] [])

$(mkTableInstances (''MediaFileDocumentT) "media_file_document")
