{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TDSDistributionPdfFile where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TDSDistributionPdfFile
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TDSDistributionPdfFileT f = TDSDistributionPdfFileT
  { batchId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fileName :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    issue :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSFileIssue)),
    matchedPersonId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    recipientType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSRecipientType)),
    s3FilePath :: (B.C f Kernel.Prelude.Text),
    sizeBytes :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    tdsDistributionRecordId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validationStatus :: (B.C f (Kernel.Prelude.Maybe Domain.Types.TDSDistributionPdfFile.TDSFileValidationStatus))
  }
  deriving (Generic, B.Beamable)

instance B.Table TDSDistributionPdfFileT where
  data PrimaryKey TDSDistributionPdfFileT f = TDSDistributionPdfFileId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TDSDistributionPdfFileId . id

type TDSDistributionPdfFile = TDSDistributionPdfFileT Identity

$(enableKVPG (''TDSDistributionPdfFileT) [('id)] [[('batchId)], [('tdsDistributionRecordId)]])

$(mkTableInstances (''TDSDistributionPdfFileT) "tds_distribution_pdf_file")
