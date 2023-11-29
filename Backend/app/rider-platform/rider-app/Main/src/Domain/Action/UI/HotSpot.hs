{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.HotSpot where

import Control.Lens
import Data.Default.Class
import Domain.Types.HotSpot
import Domain.Types.HotSpotConfig
import Domain.Types.LocationAddress as LA
import Domain.Types.Merchant
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Storage.CachedQueries.HotSpotConfig as QHotSpotConfig
import Storage.CachedQueries.Maps.LocationMapCache

frequencyUpdator ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  LatLong ->
  Maybe LA.LocationAddress ->
  TypeOfMovement ->
  m ()
frequencyUpdator merchantId latLong _address movement = do
  hotSpotConfig <- QHotSpotConfig.findConfigByMerchantId merchantId
  case hotSpotConfig of
    Just HotSpotConfig {..} -> when shouldTakeHotSpot do
      mbHotSpot <- convertToHotSpot latLong _address merchantId
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      case mbHotSpot of
        Just (HotSpot {..}) -> do
          mbTargetHotSpot :: Maybe HotSpot <- hGet makeHotSpotKey _geoHash
          case mbTargetHotSpot of
            Just foundHotSpot -> do
              let updatedHotSpot = foundHotSpot & movementLens movement %~ (+ 1)
              hSetExp makeHotSpotKey _geoHash updatedHotSpot expTime
            Nothing -> do
              let updatedGeoHash =
                    def
                      & geoHash .~ _geoHash
                      & centroidLatLong .~ _centroidLatLong
                      & movementLens movement %~ (+ 1)
                      & address .~ _address
              hSetExp makeHotSpotKey _geoHash updatedGeoHash expTime
        Nothing -> return ()
    Nothing -> return ()
