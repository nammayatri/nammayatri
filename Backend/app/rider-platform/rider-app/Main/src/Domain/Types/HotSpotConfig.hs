module Domain.Types.HotSpotConfig where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id

data HotSpotConfig = HotSpotConfig
  { id :: Id HotSpotConfig,
    hotSpotGeoHashPrecision :: Int,
    nearbyGeohashPrecision :: Int,
    blockRadius :: Int,
    minFrequencyOfHotSpot :: Int,
    weightOfManualPickup :: Int,
    weightOfManualSaved :: Int,
    weightOfAutoPickup :: Int,
    weightOfAutoSaved :: Int,
    weightOfTripStart :: Int,
    maxNumHotSpotsToShow :: Int,
    weightOfTripEnd :: Int,
    weightOfSpecialLocation :: Int,
    shouldTakeHotSpot :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, Eq)
