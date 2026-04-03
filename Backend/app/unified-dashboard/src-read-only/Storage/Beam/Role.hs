{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Role where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Data.Text
import qualified Database.Beam as B



data RoleT f
    = RoleT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
             description :: (B.C f Data.Text.Text),
             id :: (B.C f Data.Text.Text),
             name :: (B.C f Data.Text.Text),
             needsBppAccountCreation :: (B.C f Kernel.Prelude.Bool),
             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RoleT
    where data PrimaryKey RoleT f = RoleId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = RoleId . id
type Role = RoleT Identity

$(enableKVPG (''RoleT) [('id)] [])

$(mkTableInstances (''RoleT) "role")

