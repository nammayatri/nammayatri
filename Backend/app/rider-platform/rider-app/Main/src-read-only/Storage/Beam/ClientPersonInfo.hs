{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ClientPersonInfo where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ClientPersonInfoT f = ClientPersonInfoT
  { clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    rideCount :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe BecknV2.OnDemand.Enums.VehicleCategory)
  }
  deriving (Generic, B.Beamable)

instance B.Table ClientPersonInfoT where
  data PrimaryKey ClientPersonInfoT f = ClientPersonInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ClientPersonInfoId . id

type ClientPersonInfo = ClientPersonInfoT Identity

$(enableKVPG ''ClientPersonInfoT ['id] [['personId]])

$(mkTableInstances ''ClientPersonInfoT "client_person_info")
