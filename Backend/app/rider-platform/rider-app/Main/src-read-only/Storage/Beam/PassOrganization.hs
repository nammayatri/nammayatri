{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassOrganization where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PassType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PassOrganizationT f = PassOrganizationT
  { address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Data.Time.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    passEnum :: (B.C f Domain.Types.PassType.PassEnum),
    personId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Data.Time.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PassOrganizationT where
  data PrimaryKey PassOrganizationT f = PassOrganizationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassOrganizationId . id

type PassOrganization = PassOrganizationT Identity

$(enableKVPG (''PassOrganizationT) [('id)] [[('personId)]])

$(mkTableInstances (''PassOrganizationT) "pass_organization")
