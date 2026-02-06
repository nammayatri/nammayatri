{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Organization where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OrganizationT f = OrganizationT
  { createdAt :: (B.C f Data.Time.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    organizationAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    organizationName :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Data.Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OrganizationT where
  data PrimaryKey OrganizationT f = OrganizationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OrganizationId . id

type Organization = OrganizationT Identity

$(enableKVPG (''OrganizationT) [('id)] [[('personId)]])

$(mkTableInstances (''OrganizationT) "organization")
