{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.KnowledgeCenter where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified AWS.S3.Types
import qualified Database.Beam as B



data KnowledgeCenterT f
    = KnowledgeCenterT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        documentName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        fileType :: (B.C f AWS.S3.Types.FileType),
                        id :: (B.C f Kernel.Prelude.Text),
                        merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                        merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                        s3Path :: (B.C f Kernel.Prelude.Text),
                        sopType :: (B.C f Kernel.Prelude.Text),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table KnowledgeCenterT
    where data PrimaryKey KnowledgeCenterT f = KnowledgeCenterId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = KnowledgeCenterId . id
type KnowledgeCenter = KnowledgeCenterT Identity

$(enableKVPG (''KnowledgeCenterT) [('id)] [[('sopType)]])

$(mkTableInstances (''KnowledgeCenterT) "knowledge_center")

