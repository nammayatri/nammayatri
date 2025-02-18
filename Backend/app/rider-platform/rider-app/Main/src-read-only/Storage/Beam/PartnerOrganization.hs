{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PartnerOrganization where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PartnerOrganizationT f = PartnerOrganizationT
  { apiKeyEncrypted :: B.C f Kernel.Prelude.Text,
    apiKeyHash :: B.C f Kernel.External.Encryption.DbHash,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    orgId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PartnerOrganizationT where
  data PrimaryKey PartnerOrganizationT f = PartnerOrganizationId (B.C f Kernel.External.Encryption.DbHash) deriving (Generic, B.Beamable)
  primaryKey = PartnerOrganizationId . apiKeyHash

type PartnerOrganization = PartnerOrganizationT Identity

$(enableKVPG ''PartnerOrganizationT ['apiKeyHash] [['orgId]])

$(mkTableInstances ''PartnerOrganizationT "partner_organization")
