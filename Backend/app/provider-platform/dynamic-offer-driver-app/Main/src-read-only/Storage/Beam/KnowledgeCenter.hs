{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.KnowledgeCenter where

import qualified AWS.S3.Types
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data KnowledgeCenterT f = KnowledgeCenterT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    fileType :: (B.C f AWS.S3.Types.FileType),
    id :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    s3Path :: (B.C f Kernel.Prelude.Text),
    sopType :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table KnowledgeCenterT where
  data PrimaryKey KnowledgeCenterT f = KnowledgeCenterId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = KnowledgeCenterId . id

type KnowledgeCenter = KnowledgeCenterT Identity

$(enableKVPG (''KnowledgeCenterT) [('id)] [[('sopType)]])

$(mkTableInstances (''KnowledgeCenterT) "knowledge_center")
