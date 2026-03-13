{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareProduct where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.FareProduct
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data FareProductT f = FareProductT
  { area :: B.C f Lib.Types.SpecialLocation.Area,
    disableRecompute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    disableTollRecompute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    enabled :: B.C f Kernel.Prelude.Bool,
    farePolicyId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    searchSource :: B.C f Domain.Types.FareProduct.SearchSource,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    tripCategory :: B.C f Domain.Types.Common.TripCategory,
    vehicleVariant :: B.C f Domain.Types.Common.ServiceTierType
  }
  deriving (Generic, B.Beamable)

instance B.Table FareProductT where
  data PrimaryKey FareProductT f = FareProductId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareProductId . id

type FareProduct = FareProductT Identity

$(enableKVPG ''FareProductT ['id] [])

$(mkTableInstances ''FareProductT "fare_product")
