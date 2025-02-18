{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FlaggedCategory where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FlaggedCategoryT f = FlaggedCategoryT {createdAt :: B.C f Kernel.Prelude.UTCTime, id :: B.C f Kernel.Prelude.Text, name :: B.C f Kernel.Prelude.Text, updatedAt :: B.C f Kernel.Prelude.UTCTime}
  deriving (Generic, B.Beamable)

instance B.Table FlaggedCategoryT where
  data PrimaryKey FlaggedCategoryT f = FlaggedCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FlaggedCategoryId . id

type FlaggedCategory = FlaggedCategoryT Identity

$(enableKVPG ''FlaggedCategoryT ['id] [])

$(mkTableInstances ''FlaggedCategoryT "flagged_category")
