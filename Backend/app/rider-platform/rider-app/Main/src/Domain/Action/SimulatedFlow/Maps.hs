{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.SimulatedFlow.Maps
  ( autoComplete,
    addMeters,
  )
where

import qualified Data.FuzzySet as FS
import qualified Data.HashMap as HM
import qualified Kernel.External.Maps as KMaps
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified SharedLogic.MerchantConfig as SMC
import qualified Tools.Maps as Maps

autoComplete :: (Redis.HedisFlow m r, MonadFlow m) => Maps.AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete req = do
  cachedBookingDetails :: [(Text, SMC.BookingInfoStore)] <- Redis.hGetAll SMC.bookingInfoStoreKey
  now <- getCurrentTime
  let cachedBookingMap = HM.fromList cachedBookingDetails
      fuzzySet = FS.fromList $ HM.keys cachedBookingMap
      hits = map snd $ FS.get fuzzySet req.input
      matchingAddresses = map (\key -> (key, HM.lookup key cachedBookingMap)) hits
      predictions = map (\bi -> KMaps.Prediction bi.description bi.placeId Nothing) . catMaybes $ map snd matchingAddresses
  mapM_
    ( \(key, value) ->
        case value of
          Just val -> Redis.hSetExp SMC.bookingInfoStoreKey key (val {SMC.lastAccessed = now}) 1209600
          Nothing -> Redis.hDel SMC.bookingInfoStoreKey [key]
    )
    matchingAddresses
  pure $ KMaps.AutoCompleteResp {..}

addMeters :: Double -> Maps.LatLong -> Maps.LatLong
addMeters meters latLong =
  -- number of km per degree = ~111km
  -- 111.32km = 111320.0m
  let coef = meters / 111320.0
      newLat = latLong.lat + coef
      newLong = latLong.lon + coef / cos (latLong.lat * 0.01745) -- pi / 180 ~= 0.01745
   in Maps.LatLong {lat = newLat, lon = newLong}
