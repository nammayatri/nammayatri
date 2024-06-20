{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ClientPersonInfo where

import qualified Database.Beam as B
import qualified Domain.Types.BecknConfig
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ClientPersonInfoT f = ClientPersonInfoT
  { id :: B.C f Kernel.Prelude.Text,
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    personId :: B.C f Kernel.Prelude.Text,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.BecknConfig.VehicleCategory),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    rideCount :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ClientPersonInfoT where
  data PrimaryKey ClientPersonInfoT f = ClientPersonInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ClientPersonInfoId . id

type ClientPersonInfo = ClientPersonInfoT Identity

$(enableKVPG ''ClientPersonInfoT ['id] [['personId]])

$(mkTableInstances ''ClientPersonInfoT "client_person_info")
