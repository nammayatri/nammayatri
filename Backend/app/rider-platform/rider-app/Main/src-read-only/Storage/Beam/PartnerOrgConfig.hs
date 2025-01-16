{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PartnerOrgConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.PartnerOrgConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PartnerOrgConfigT f = PartnerOrgConfigT
  { configJSON :: B.C f Data.Aeson.Value,
    configType :: B.C f Domain.Types.Extra.PartnerOrgConfig.ConfigType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    partnerOrgId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PartnerOrgConfigT where
  data PrimaryKey PartnerOrgConfigT f = PartnerOrgConfigId (B.C f Domain.Types.Extra.PartnerOrgConfig.ConfigType) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PartnerOrgConfigId <$> configType <*> partnerOrgId

type PartnerOrgConfig = PartnerOrgConfigT Identity

$(enableKVPG ''PartnerOrgConfigT ['configType, 'partnerOrgId] [])

$(mkTableInstances ''PartnerOrgConfigT "partner_org_config")
