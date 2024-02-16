module Domain.Types.HotSpotConfig where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id

data HotSpotConfig = HotSpotConfig
  { id :: Id HotSpotConfig,
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
    shouldTakeHotSpot :: Bool,
    shouldSaveSearchHotSpot :: Bool,
    hotSpotRadius :: Double,
    precisionToSetGeohash :: Int,
    precisionToGetGeohash :: Int,
    precisionToFilterGeohash :: Int,
    maxGeoHashToFilter :: Int,
    hotSpotExpiry :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, Eq)
