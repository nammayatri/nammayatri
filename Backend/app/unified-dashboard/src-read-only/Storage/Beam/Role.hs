{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Role where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RoleT f = RoleT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    name :: (B.C f Data.Text.Text),
    needsBppAccountCreation :: (B.C f Kernel.Prelude.Bool),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RoleT where
  data PrimaryKey RoleT f = RoleId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = RoleId . id

type Role = RoleT Identity

$(enableKVPG (''RoleT) [('id)] [])

$(mkTableInstances (''RoleT) "role")
