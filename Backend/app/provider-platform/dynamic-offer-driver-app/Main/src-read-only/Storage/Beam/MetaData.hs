{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MetaData where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MetaDataT f = MetaDataT
  { appPermissions :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    device :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    deviceDateTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    deviceOS :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    driverId :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MetaDataT where
  data PrimaryKey MetaDataT f = MetaDataId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = MetaDataId . driverId

type MetaData = MetaDataT Identity

$(enableKVPG ''MetaDataT ['driverId] [])

$(mkTableInstancesWithTModifier ''MetaDataT "meta_data" [("deviceOS", "device_o_s")])
