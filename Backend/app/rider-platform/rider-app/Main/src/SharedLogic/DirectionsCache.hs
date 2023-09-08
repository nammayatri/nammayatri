{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.DirectionsCache
  ( Maps.GetRoutesReq,
    Maps.GetRoutesResp,
    getRoutes,
  )
where

import Data.Geohash as DG
import Data.List.NonEmpty as NE
import Data.Text (pack)
import Data.Time as DT
  ( LocalTime (localTimeOfDay),
    TimeZone (TimeZone),
    utcToLocalTime,
  )
import Domain.Types.Maps.DirectionsCache as DC
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Merchant.MerchantConfigNew (Slot)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.Maps.DirectionsCache as DCC
import qualified Storage.CachedQueries.Maps.DirectionsCache as DQ
import qualified Storage.CachedQueries.Merchant.MerchantConfigNew as QMCN
import qualified Tools.Maps as Maps

getRoutes :: (ServiceFlow m r, EsqDBReplicaFlow m r) => Id Merchant.Merchant -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes merchantId req = do
  merchantConfig <- QMCN.findByMerchantId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let origin = NE.head req.waypoints
  let dest = NE.last req.waypoints
  originGeoHash <- fmap pack $ fromMaybeM (InternalError "Failed to compute Origin GeoHash") $ DG.encode merchantConfig.geoHashPrecisionValue (origin.lat, origin.lon) -- This default case will never happen as we are getting lat long from Google and hence a valid geo hash will always be possible for a valid lat long.
  destGeoHash <- fmap pack $ fromMaybeM (InternalError "Failed to compute Destination GeoHash") $ DG.encode merchantConfig.geoHashPrecisionValue (dest.lat, dest.lon) -- This default case will never happen as we are getting lat long from Google and hence a valid geo hash will always be possible for a valid lat long.
  timeSlot <- getSlot merchantId
  case timeSlot of
    Just tmeSlt ->
      do
        cachedResp <- DQ.findRoute originGeoHash destGeoHash tmeSlt
        case cachedResp of
          Just resp -> return [resp.response]
          Nothing -> callDirectionsApi merchantId req originGeoHash destGeoHash tmeSlt
    Nothing ->
      Maps.getRoutes merchantId req

callDirectionsApi :: ServiceFlow m r => Id Merchant.Merchant -> Maps.GetRoutesReq -> Text -> Text -> Int -> m Maps.GetRoutesResp
callDirectionsApi merchantId req originGeoHash destGeoHash timeSlot = do
  resp <- Maps.getRoutes merchantId req
  if null resp
    then throwError $ InternalError "Null response from Directions API" -- This case will never occure unless Google's Direction API Fails.
    else do
      let (cachedResp : _) = resp
      directionsCache <- convertToDirCache originGeoHash destGeoHash timeSlot cachedResp
      DQ.create directionsCache
      DCC.cacheDirectionsResponse directionsCache
      return resp

getSlot :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant.Merchant -> m (Maybe Int)
getSlot merchantId = do
  utcTime <- getLocalCurrentTime 19800
  let istTime = utcToLocalTime (TimeZone 0 False "") utcTime
  let timeOfDay = localTimeOfDay istTime
  slots <- fmap (.dirCacheSlot) $ QMCN.findByMerchantId merchantId >>= fromMaybeM (InternalError "Error in fetching configs from Database")
  return $ matchSlot timeOfDay slots

matchSlot :: TimeOfDay -> [Slot] -> Maybe Int
matchSlot _ [] = Nothing
matchSlot currTime (slot : slots)
  | slot.startTime <= currTime && currTime < slot.endTime = Just $ slot.slot
  | otherwise = matchSlot currTime slots

convertToDirCache :: (MonadGuid m, MonadTime m) => Text -> Text -> Int -> Maps.RouteInfo -> m DirectionsCache
convertToDirCache originGeoHash destGeoHash timeSlot cachedResp = do
  id <- generateGUID
  localTime <- getLocalCurrentTime 19800
  let res =
        DC.DirectionsCache
          { id,
            originHash = originGeoHash,
            destHash = destGeoHash,
            slot = timeSlot,
            response = cachedResp,
            createdAt = localTime
          }
  return res
