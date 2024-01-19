{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSearch where

import qualified Database.Beam as B
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data FRFSSearchT f = FRFSSearchT
  { fromStationId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    quantity :: B.C f Kernel.Prelude.Int,
    riderId :: B.C f Kernel.Prelude.Text,
    toStationId :: B.C f Kernel.Prelude.Text,
    vehicleType :: B.C f Domain.Types.Station.FRFSVehicleType,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSearchT where
  data PrimaryKey FRFSSearchT f = FRFSSearchId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = FRFSSearchId . id

type FRFSSearch = FRFSSearchT Identity

$(enableKVPG ''FRFSSearchT ['id] [])

$(mkTableInstances ''FRFSSearchT "frfs_search")
