{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.HotSpotConfig where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data HotSpotConfig = HotSpotConfig
  { blockRadius :: Kernel.Prelude.Int,
    hotSpotExpiry :: Kernel.Prelude.Int,
    hotSpotRadius :: Kernel.Prelude.Double,
    id :: Kernel.Types.Id.Id Domain.Types.HotSpotConfig.HotSpotConfig,
    maxGeoHashToFilter :: Kernel.Prelude.Int,
    maxNumHotSpotsToShow :: Kernel.Prelude.Int,
    minFrequencyOfHotSpot :: Kernel.Prelude.Int,
    precisionToFilterGeohash :: Kernel.Prelude.Int,
    precisionToGetGeohash :: Kernel.Prelude.Int,
    precisionToSetGeohash :: Kernel.Prelude.Int,
    shouldSaveSearchHotSpot :: Kernel.Prelude.Bool,
    shouldTakeHotSpot :: Kernel.Prelude.Bool,
    weightOfAutoPickup :: Kernel.Prelude.Int,
    weightOfAutoSaved :: Kernel.Prelude.Int,
    weightOfManualPickup :: Kernel.Prelude.Int,
    weightOfManualSaved :: Kernel.Prelude.Int,
    weightOfSpecialLocation :: Kernel.Prelude.Int,
    weightOfTripEnd :: Kernel.Prelude.Int,
    weightOfTripStart :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
