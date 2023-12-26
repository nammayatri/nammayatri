{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.HotSpot where

import Control.Lens
import Data.Aeson
import Data.Default.Class
import qualified Data.Geohash as DG
import Data.Text
import Domain.Types.HotSpotConfig
import Domain.Types.LocationAddress as LA
import Domain.Types.Merchant
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.CachedQueries.HotSpotConfig as QHotSpotConfig

data HotSpot = HotSpot
  { _geoHash :: Text,
    _centroidLatLong :: LatLong,
    _manualMovedPickup :: Int,
    _manualMovedSaved :: Int,
    _nonManualMovedPickup :: Int,
    _nonManualMovedSaved :: Int,
    _tripStart :: Int,
    _tripEnd :: Int,
    _specialLocation :: Int,
    _address :: Maybe LA.LocationAddress,
    _updatedAt :: Maybe UTCTime
  }
  deriving (Generic, ToSchema, Show, Eq)

instance FromJSON HotSpot where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = stripPrefixUnderscoreIfAny}

instance ToJSON HotSpot where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = stripPrefixUnderscoreIfAny}

stripPrefixUnderscoreIfAny :: String -> String
stripPrefixUnderscoreIfAny ('_' : xs) = xs
stripPrefixUnderscoreIfAny xs = xs

instance Default HotSpot where
  def =
    HotSpot
      { _geoHash = mempty,
        _centroidLatLong = LatLong 0 0,
        _manualMovedPickup = 0,
        _manualMovedSaved = 0,
        _nonManualMovedPickup = 0,
        _nonManualMovedSaved = 0,
        _tripStart = 0,
        _tripEnd = 0,
        _specialLocation = 0,
        _address = Nothing,
        _updatedAt = Nothing
      }

makeLenses ''HotSpot

movementLens :: TypeOfMovement -> Lens' HotSpot Int
movementLens ManualPickup = manualMovedPickup
movementLens NonManualPickup = nonManualMovedPickup
movementLens ManualSaved = manualMovedSaved
movementLens NonManualSaved = nonManualMovedSaved
movementLens TripStart = tripStart
movementLens TripEnd = tripEnd
movementLens SpecialLocation = specialLocation

data TypeOfMovement = ManualPickup | NonManualPickup | ManualSaved | NonManualSaved | TripStart | TripEnd | SpecialLocation deriving (Generic, Eq, Show)

data HotSpotInfo = HotSpotInfo
  { _geoHash :: Text,
    _centroidLatLong :: LatLong,
    _address :: Maybe LA.LocationAddress
  }
  deriving (Generic, ToSchema, Show, Eq)

instance FromJSON HotSpotInfo where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = stripPrefixUnderscoreIfAny}

instance ToJSON HotSpotInfo where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = stripPrefixUnderscoreIfAny}

data HotSpotResponse = HotSpotResponse
  { hotSpotInfo :: [HotSpotInfo],
    blockRadius :: Maybe Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

convertToHotSpot ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  LatLong ->
  Maybe LA.LocationAddress ->
  Id Merchant ->
  m (Maybe HotSpot)
convertToHotSpot LatLong {..} _address merchantId = do
  hotSpotConfig <- QHotSpotConfig.findConfigByMerchantId merchantId
  case hotSpotConfig of
    Just HotSpotConfig {..} -> do
      let mbGeoHash = DG.encode precisionToSetGeohash (lat, lon)
      case mbGeoHash of
        Just geo -> do
          let mbDecodedCentroid = DG.decode geo :: Maybe (Double, Double)
          case mbDecodedCentroid of
            Just decodedCentroid ->
              return . Just $
                def
                  & geoHash .~ pack geo
                  & centroidLatLong .~ uncurry LatLong decodedCentroid
                  & address .~ _address
            Nothing ->
              return Nothing
        Nothing ->
          return Nothing
    Nothing -> return Nothing
