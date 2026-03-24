{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PartnerOrganization where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PartnerOrganizationT f
    = PartnerOrganizationT {apiKeyEncrypted :: (B.C f Kernel.Prelude.Text),
                            apiKeyHash :: (B.C f Kernel.External.Encryption.DbHash),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            merchantId :: (B.C f Kernel.Prelude.Text),
                            name :: (B.C f Kernel.Prelude.Text),
                            orgId :: (B.C f Kernel.Prelude.Text),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PartnerOrganizationT
    where data PrimaryKey PartnerOrganizationT f = PartnerOrganizationId (B.C f Kernel.External.Encryption.DbHash) deriving (Generic, B.Beamable)
          primaryKey = PartnerOrganizationId . apiKeyHash
type PartnerOrganization = PartnerOrganizationT Identity

$(enableKVPG (''PartnerOrganizationT) [('apiKeyHash)] [[('orgId)]])

$(mkTableInstances (''PartnerOrganizationT) "partner_organization")

