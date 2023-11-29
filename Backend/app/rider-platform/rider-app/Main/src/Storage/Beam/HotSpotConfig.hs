{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.HotSpotConfig where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data HotSpotConfigT f = HotSpotConfigT
  { id :: B.C f Text,
    blockRadius :: B.C f Int,
    minFrequencyOfHotSpot :: B.C f Int,
    weightOfManualPickup :: B.C f Int,
    weightOfManualSaved :: B.C f Int,
    weightOfAutoPickup :: B.C f Int,
    weightOfAutoSaved :: B.C f Int,
    weightOfTripStart :: B.C f Int,
    weightOfTripEnd :: B.C f Int,
    weightOfSpecialLocation :: B.C f Int,
    shouldTakeHotSpot :: B.C f Bool,
    maxNumHotSpotsToShow :: B.C f Int,
    shouldSaveSearchHotSpot :: B.C f Bool,
    hotSpotRadius :: B.C f Double,
    precisionToSetGeohash :: B.C f Int,
    precisionToGetGeohash :: B.C f Int,
    precisionToFilterGeohash :: B.C f Int,
    maxGeoHashToFilter :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table HotSpotConfigT where
  data PrimaryKey HotSpotConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type HotSpotConfig = HotSpotConfigT Identity

defaultHotSpotConfig :: HotSpotConfig
defaultHotSpotConfig =
  HotSpotConfigT
    { id = "",
      blockRadius = 0,
      minFrequencyOfHotSpot = 0,
      weightOfManualPickup = 0,
      weightOfManualSaved = 0,
      weightOfAutoPickup = 0,
      weightOfAutoSaved = 0,
      weightOfTripStart = 0,
      weightOfTripEnd = 0,
      weightOfSpecialLocation = 0,
      shouldTakeHotSpot = False,
      maxNumHotSpotsToShow = 0,
      shouldSaveSearchHotSpot = False,
      hotSpotRadius = 0,
      precisionToSetGeohash = 0,
      precisionToGetGeohash = 0,
      precisionToFilterGeohash = 0,
      maxGeoHashToFilter = 0
    }

$(enableKVPG ''HotSpotConfigT ['id] [])

$(mkTableInstances ''HotSpotConfigT "hot_spot_config")
