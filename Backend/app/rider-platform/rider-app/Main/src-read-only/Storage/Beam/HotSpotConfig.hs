{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.HotSpotConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data HotSpotConfigT f = HotSpotConfigT
  { blockRadius :: B.C f Kernel.Prelude.Int,
    hotSpotExpiry :: B.C f Kernel.Prelude.Int,
    hotSpotRadius :: B.C f Kernel.Prelude.Double,
    id :: B.C f Kernel.Prelude.Text,
    maxGeoHashToFilter :: B.C f Kernel.Prelude.Int,
    maxNumHotSpotsToShow :: B.C f Kernel.Prelude.Int,
    minFrequencyOfHotSpot :: B.C f Kernel.Prelude.Int,
    precisionToFilterGeohash :: B.C f Kernel.Prelude.Int,
    precisionToGetGeohash :: B.C f Kernel.Prelude.Int,
    precisionToSetGeohash :: B.C f Kernel.Prelude.Int,
    shouldSaveSearchHotSpot :: B.C f Kernel.Prelude.Bool,
    shouldTakeHotSpot :: B.C f Kernel.Prelude.Bool,
    weightOfAutoPickup :: B.C f Kernel.Prelude.Int,
    weightOfAutoSaved :: B.C f Kernel.Prelude.Int,
    weightOfManualPickup :: B.C f Kernel.Prelude.Int,
    weightOfManualSaved :: B.C f Kernel.Prelude.Int,
    weightOfSpecialLocation :: B.C f Kernel.Prelude.Int,
    weightOfTripEnd :: B.C f Kernel.Prelude.Int,
    weightOfTripStart :: B.C f Kernel.Prelude.Int
  }
  deriving (Generic, B.Beamable)

instance B.Table HotSpotConfigT where
  data PrimaryKey HotSpotConfigT f = HotSpotConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = HotSpotConfigId . id

type HotSpotConfig = HotSpotConfigT Identity

$(enableKVPG ''HotSpotConfigT ['id] [])

$(mkTableInstances ''HotSpotConfigT "hot_spot_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''HotSpotConfigT)
