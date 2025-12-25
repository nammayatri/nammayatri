{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BlackListOrg where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Tools.Beam.UtilsTH

data BlackListOrgT f = BlackListOrgT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    domain :: B.C f Kernel.Types.Beckn.Domain.Domain,
    id :: B.C f Kernel.Prelude.Text,
    subscriberId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BlackListOrgT where
  data PrimaryKey BlackListOrgT f = BlackListOrgId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BlackListOrgId . id

type BlackListOrg = BlackListOrgT Identity

$(enableKVPG ''BlackListOrgT ['id] [['subscriberId]])

$(mkTableInstancesWithTModifier ''BlackListOrgT "black_list_org" [("subscriberId", "subscriber_id")])
