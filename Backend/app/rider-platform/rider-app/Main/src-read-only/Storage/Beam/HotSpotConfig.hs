{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.HotSpotConfig where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data HotSpotConfigT f = HotSpotConfigT
  { id :: B.C f Kernel.Prelude.Text,
    blockRadius :: B.C f Kernel.Prelude.Int,
    minFrequencyOfHotSpot :: B.C f Kernel.Prelude.Int,
    weightOfManualPickup :: B.C f Kernel.Prelude.Int,
    weightOfManualSaved :: B.C f Kernel.Prelude.Int,
    weightOfAutoPickup :: B.C f Kernel.Prelude.Int,
    weightOfAutoSaved :: B.C f Kernel.Prelude.Int,
    weightOfTripStart :: B.C f Kernel.Prelude.Int,
    maxNumHotSpotsToShow :: B.C f Kernel.Prelude.Int,
    weightOfTripEnd :: B.C f Kernel.Prelude.Int,
    weightOfSpecialLocation :: B.C f Kernel.Prelude.Int,
    shouldTakeHotSpot :: B.C f Kernel.Prelude.Bool,
    shouldSaveSearchHotSpot :: B.C f Kernel.Prelude.Bool,
    hotSpotRadius :: B.C f Kernel.Prelude.Double,
    precisionToSetGeohash :: B.C f Kernel.Prelude.Int,
    precisionToGetGeohash :: B.C f Kernel.Prelude.Int,
    precisionToFilterGeohash :: B.C f Kernel.Prelude.Int,
    maxGeoHashToFilter :: B.C f Kernel.Prelude.Int,
    hotSpotExpiry :: B.C f Kernel.Prelude.Int
  }
  deriving (Generic, B.Beamable)

instance B.Table HotSpotConfigT where
  data PrimaryKey HotSpotConfigT f = HotSpotConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = HotSpotConfigId . id

type HotSpotConfig = HotSpotConfigT Identity

$(enableKVPG ''HotSpotConfigT ['id] [])

$(mkTableInstances ''HotSpotConfigT "hot_spot_config")
