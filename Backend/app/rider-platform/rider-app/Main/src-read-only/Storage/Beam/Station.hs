{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Station where

import qualified Database.Beam as B
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data StationT f = StationT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    code :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f Domain.Types.Station.FRFSVehicleType,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table StationT where
  data PrimaryKey StationT f = StationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StationId . id

type Station = StationT Identity

$(enableKVPG ''StationT ['id] [['code]])

$(mkTableInstances ''StationT "station")

{-
	DSL Source Link: file://./../../../spec/Storage/FrfsTicket.yaml
-}
