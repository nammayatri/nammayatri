{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TDSDistributionPdfFile where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TDSDistributionPdfFileT f = TDSDistributionPdfFileT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fileName :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    s3FilePath :: (B.C f Kernel.Prelude.Text),
    tdsDistributionRecordId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TDSDistributionPdfFileT where
  data PrimaryKey TDSDistributionPdfFileT f = TDSDistributionPdfFileId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TDSDistributionPdfFileId . id

type TDSDistributionPdfFile = TDSDistributionPdfFileT Identity

$(enableKVPG (''TDSDistributionPdfFileT) [('id)] [[('tdsDistributionRecordId)]])

$(mkTableInstances (''TDSDistributionPdfFileT) "tds_distribution_pdf_file")
