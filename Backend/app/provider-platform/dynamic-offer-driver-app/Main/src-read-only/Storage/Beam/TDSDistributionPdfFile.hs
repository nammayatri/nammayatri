{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TDSDistributionPdfFile where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data TDSDistributionPdfFileT f
    = TDSDistributionPdfFileT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               fileName :: (B.C f Kernel.Prelude.Text),
                               id :: (B.C f Kernel.Prelude.Text),
                               s3FilePath :: (B.C f Kernel.Prelude.Text),
                               tdsDistributionRecordId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TDSDistributionPdfFileT
    where data PrimaryKey TDSDistributionPdfFileT f = TDSDistributionPdfFileId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TDSDistributionPdfFileId . id
type TDSDistributionPdfFile = TDSDistributionPdfFileT Identity

$(enableKVPG (''TDSDistributionPdfFileT) [('id)] [[('tdsDistributionRecordId)]])

$(mkTableInstances (''TDSDistributionPdfFileT) "tds_distribution_pdf_file")

