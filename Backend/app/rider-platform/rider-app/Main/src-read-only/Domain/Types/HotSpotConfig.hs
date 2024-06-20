{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.HotSpotConfig where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data HotSpotConfig = HotSpotConfig
  { id :: Kernel.Types.Id.Id Domain.Types.HotSpotConfig.HotSpotConfig,
    blockRadius :: Kernel.Prelude.Int,
    minFrequencyOfHotSpot :: Kernel.Prelude.Int,
    weightOfManualPickup :: Kernel.Prelude.Int,
    weightOfManualSaved :: Kernel.Prelude.Int,
    weightOfAutoPickup :: Kernel.Prelude.Int,
    weightOfAutoSaved :: Kernel.Prelude.Int,
    weightOfTripStart :: Kernel.Prelude.Int,
    maxNumHotSpotsToShow :: Kernel.Prelude.Int,
    weightOfTripEnd :: Kernel.Prelude.Int,
    weightOfSpecialLocation :: Kernel.Prelude.Int,
    shouldTakeHotSpot :: Kernel.Prelude.Bool,
    shouldSaveSearchHotSpot :: Kernel.Prelude.Bool,
    hotSpotRadius :: Kernel.Prelude.Double,
    precisionToSetGeohash :: Kernel.Prelude.Int,
    precisionToGetGeohash :: Kernel.Prelude.Int,
    precisionToFilterGeohash :: Kernel.Prelude.Int,
    maxGeoHashToFilter :: Kernel.Prelude.Int,
    hotSpotExpiry :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
