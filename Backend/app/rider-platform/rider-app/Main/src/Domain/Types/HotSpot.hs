module Domain.Types.HotSpot where

import qualified Data.Geohash as DG
import Data.Text
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common
import Tools.Metrics

data HotSpot = HotSpot
  { geoHash :: Maybe Text,
    frequency :: Int,
    centroidLatLong :: Maybe LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data HotSpotResponse = HotSpotResponse
  { hotSpots :: [HotSpot],
    blockRadius :: HighPrecMeters
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

convertToHotSpot ::
  ( CoreMetrics m,
    HasFlowEnv m r '["hotSpotGeoHashPrecision" ::: Int]
  ) =>
  LatLong ->
  m HotSpot
convertToHotSpot LatLong {..} = do
  geoHashLength <- asks (.hotSpotGeoHashPrecision)
  let mbGeoHash = DG.encode geoHashLength (lat, lon)

  let frequency = 1
  case mbGeoHash of
    Just geo -> do
      let mbDecodedCentroid = DG.decode geo :: Maybe (Double, Double)
      case mbDecodedCentroid of
        Just decodedCentroid ->
          return $
            HotSpot
              { geoHash = Just (pack geo),
                centroidLatLong = Just (uncurry LatLong decodedCentroid),
                ..
              }
        Nothing ->
          return $
            HotSpot
              { geoHash = Just (pack geo),
                centroidLatLong = Nothing,
                ..
              }
    Nothing ->
      return
        HotSpot
          { geoHash = Nothing,
            centroidLatLong = Nothing,
            ..
          }
