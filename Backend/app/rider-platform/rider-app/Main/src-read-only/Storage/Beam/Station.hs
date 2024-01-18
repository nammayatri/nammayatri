{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Station where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data StationT f = StationT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    code :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    name :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table StationT where
  data PrimaryKey StationT f = StationId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = StationId . id

type Station = StationT Identity

$(enableKVPG ''StationT ['id] [])

$(mkTableInstances ''StationT "station")
