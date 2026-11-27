{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Beam.PermitApplicationDocument where

import qualified Data.Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Permit.Domain.Types.PermitApplicationDocument

data PermitApplicationDocumentT f = PermitApplicationDocumentT
  { applicationId :: (B.C f Kernel.Prelude.Text),
    contentType :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    documentType :: (B.C f Lib.Permit.Domain.Types.PermitApplicationDocument.PermitDocumentType),
    fileHash :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    lifetime :: (B.C f Kernel.Prelude.Bool),
    s3Path :: (B.C f Kernel.Prelude.Text),
    sizeBytes :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validUntil :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day))
  }
  deriving (Generic, B.Beamable)

instance B.Table PermitApplicationDocumentT where
  data PrimaryKey PermitApplicationDocumentT f = PermitApplicationDocumentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PermitApplicationDocumentId . id

type PermitApplicationDocument = PermitApplicationDocumentT Identity

$(enableKVPG (''PermitApplicationDocumentT) [('id)] [[('applicationId)]])

$(mkTableInstancesGenericSchema (''PermitApplicationDocumentT) "permit_application_document")
