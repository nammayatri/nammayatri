{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Person where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonT f = PersonT
  { approvedBy :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    emailEncrypted :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    emailHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    firstName :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    lastName :: (B.C f Data.Text.Text),
    mobileCountryCode :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    mobileNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    mobileNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    passwordHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    passwordUpdatedAt :: (B.C f Kernel.Prelude.UTCTime),
    receiveNotification :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    rejectedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    rejectedBy :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    rejectionReason :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    roleId :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    verified :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool))
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonId . id

type Person = PersonT Identity

$(enableKVPG (''PersonT) [('id)] [])

$(mkTableInstances (''PersonT) "person")
