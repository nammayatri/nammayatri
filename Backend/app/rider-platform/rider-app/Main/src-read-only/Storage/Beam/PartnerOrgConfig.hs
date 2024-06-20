{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PartnerOrgConfig where

import qualified Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.Extra.PartnerOrgConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PartnerOrgConfigT f = PartnerOrgConfigT
  { partnerOrgId :: B.C f Kernel.Prelude.Text,
    configJSON :: B.C f Data.Aeson.Value,
    configType :: B.C f Domain.Types.Extra.PartnerOrgConfig.ConfigType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PartnerOrgConfigT where
  data PrimaryKey PartnerOrgConfigT f = PartnerOrgConfigId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Extra.PartnerOrgConfig.ConfigType) deriving (Generic, B.Beamable)
  primaryKey = PartnerOrgConfigId <$> partnerOrgId <*> configType

type PartnerOrgConfig = PartnerOrgConfigT Identity

$(enableKVPG ''PartnerOrgConfigT ['partnerOrgId, 'configType] [])

$(mkTableInstances ''PartnerOrgConfigT "partner_org_config")
