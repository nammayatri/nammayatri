{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PartnerOrgStation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PartnerOrgStationT f = PartnerOrgStationT
  { name :: B.C f Kernel.Prelude.Text,
    partnerOrgId :: B.C f Kernel.Prelude.Text,
    partnerOrgStationId :: B.C f Kernel.Prelude.Text,
    stationId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PartnerOrgStationT where
  data PrimaryKey PartnerOrgStationT f = PartnerOrgStationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PartnerOrgStationId <$> partnerOrgId <*> partnerOrgStationId

type PartnerOrgStation = PartnerOrgStationT Identity

$(enableKVPG ''PartnerOrgStationT ['partnerOrgId, 'partnerOrgStationId] [])

$(mkTableInstances ''PartnerOrgStationT "partner_org_station")
