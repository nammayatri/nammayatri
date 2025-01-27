{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.WhiteListOrg where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Tools.Beam.UtilsTH

data WhiteListOrgT f = WhiteListOrgT
  { domain :: B.C f Kernel.Types.Beckn.Domain.Domain,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    subscriberId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table WhiteListOrgT where
  data PrimaryKey WhiteListOrgT f = WhiteListOrgId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WhiteListOrgId . id

type WhiteListOrg = WhiteListOrgT Identity

$(enableKVPG ''WhiteListOrgT ['id] [])

$(mkTableInstancesWithTModifier ''WhiteListOrgT "white_list_org" [("subscriberId", "subscriber_id")])
