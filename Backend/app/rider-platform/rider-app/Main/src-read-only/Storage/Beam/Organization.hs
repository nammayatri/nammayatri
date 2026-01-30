{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Organization where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OrganizationT f = OrganizationT
  { contactName :: (B.C f Kernel.Prelude.Text),
    contactPhoneNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    contactPhoneNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    contactRole :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    organizationAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    organizationName :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OrganizationId . id

type Organization = OrganizationT Identity

$(enableKVPG (''OrganizationT) [('id)] [[('organizationName)]])

$(mkTableInstances (''OrganizationT) "organization")
