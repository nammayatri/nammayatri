{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Maps.Interface.MMI
  ( autoSuggest,
    getDistanceMatrix,
    getRoutes,
  )
where

import qualified Data.List.Extra as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import EulerHS.Prelude
import GHC.Float (double2Int)
-- import Kernel.Types.CommonImport
import Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.CommonImport
import Kernel.Types.Error
import Kernel.Utils.Common (logTagWarning)
import Kernel.Utils.Error.Throwing
import Lib.Encryption
import qualified Lib.Maps.Google.PolyLinePoints as PP
import Lib.Maps.HasCoordinates (HasCoordinates (..))
import Lib.Maps.Interface.Types as IT
import Lib.Maps.MMI.AutoSuggest as MMI
import Lib.Maps.MMI.Config
import Lib.Maps.MMI.DistanceMatrix as MMI
import Lib.Maps.MMI.MMIAuthToken as MMIAuthToken
import qualified Lib.Maps.MMI.MapsClient.Types as MMI
import qualified Lib.Maps.MMI.MapsClient.Types as MMITypes
import Lib.Maps.MMI.Routes as MMI

autoSuggest ::
  ( EncFlow m r,
    CoreMetrics m,
    Redis.HedisFlow m r,
    MonadFlow m
  ) =>
  MMICfg ->
  IT.AutoCompleteReq ->
  m IT.AutoCompleteResp
autoSuggest mmiCfg AutoCompleteReq {..} = do
  let query = input
      loc = location
      region = "ind"
      lang = language
      mapsUrl = mmiCfg.mmiNonKeyUrl
  token <- MMIAuthToken.getTokenText mmiCfg
  res <- MMI.mmiAutoSuggest mapsUrl (Just $ MMITypes.MMIAuthToken token) query loc region lang
  let predictions = map (\MMITypes.SuggestedLocations {..} -> Prediction {placeId = Just eLoc, description = placeName <> " " <> placeAddress}) res.suggestedLocations
  return $ AutoCompleteResp predictions

getDistanceMatrix ::
  ( EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    HasCoordinates a,
    HasCoordinates b
  ) =>
  MMICfg ->
  IT.GetDistancesReq a b ->
  m (NonEmpty (IT.GetDistanceResp a b))
getDistanceMatrix mmiCfg GetDistancesReq {..} = do
  key <- decrypt mmiCfg.mmiApiKey
  let limitedOriginObjectsList = splitListByAPICap origins
      limitedDestinationObjectsList = splitListByAPICap destinations
      mapsUrl = mmiCfg.mmiKeyUrl
  res <- concatForM limitedOriginObjectsList $ \limitedOriginObjects ->
    concatForM limitedDestinationObjectsList $ \limitedDestinationObjects -> do
      let limitedOriginPlaces = map getCoordinates limitedOriginObjects
          limitedDestinationPlaces = map getCoordinates limitedDestinationObjects
          lOrigin = length limitedOriginObjects
          lDest = length limitedDestinationObjects
          strOrig = map show [0 .. lOrigin - 1]
          strDest = map show [lOrigin .. (lOrigin + lDest - 1)]
          origParam = T.intercalate ";" strOrig
          origDest = T.intercalate ";" strDest
          placesList = (++) limitedOriginPlaces limitedDestinationPlaces
          coordinatesList = map latLongToText placesList
          coordinates = T.intercalate ";" coordinatesList
      MMI.mmiDistanceMatrix mapsUrl key coordinates (Just origParam) (Just origDest)
        >>= parseDistanceMatrixResp lOrigin lDest limitedOriginObjects limitedDestinationObjects
  case res of
    [] -> throwError (InternalError "Empty MMI.getDistances result.")
    (a : xs) -> return $ a :| xs
  where
    splitListByAPICap inputList = do
      List.chunksOf 50 $ toList inputList

latLongToText :: LatLong -> Text
latLongToText LatLong {..} = show lon <> "," <> show lat

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- orig, dest, distance, duration, status
parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  Int ->
  Int ->
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  m [IT.GetDistanceResp a b]
parseDistanceMatrixResp lOrigin lDest listSrc listDest distanceMatrixResp = do
  let lst = cartProd [0 .. (lOrigin - 1)] [0 .. (lDest - 1)]
  return $ map (buildResp listSrc listDest distanceMatrixResp) lst

buildResp ::
  [a] ->
  [b] ->
  MMITypes.DistanceMatrixResp ->
  (Int, Int) ->
  IT.GetDistanceResp a b
buildResp listSrc listDest distanceMatrixResp pair =
  GetDistanceResp
    { origin = listSrc !! fst pair,
      destination = listDest !! snd pair,
      distance = floor $ (distanceMatrixResp.results.distances !! fst pair) !! snd pair,
      duration = floor $ (distanceMatrixResp.results.durations !! fst pair) !! snd pair,
      status = distanceMatrixResp.results.code
    }

getRoutes ::
  ( EncFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  MMICfg ->
  IT.GetRoutesReq ->
  m IT.GetRoutesResp
getRoutes mmiCfg req = do
  key <- decrypt mmiCfg.mmiApiKey
  let origin = latLongToText (NE.head req.waypoints)
      destination = latLongToText (NE.last req.waypoints)
      points = origin <> ";" <> destination
      mapsUrl = mmiCfg.mmiKeyUrl
  resp <- MMI.mmiRoute mapsUrl key points
  traverse (mkRoute req resp) resp.routes

mkRoute ::
  (MonadFlow m) =>
  IT.GetRoutesReq ->
  MMI.RouteResponse ->
  MMI.Routes ->
  m IT.RouteInfo
mkRoute req resp route = do
  let bound = Nothing
  if null route.legs
    then do
      logTagWarning "MMIRoutes" ("Empty route.legs, " <> show req)
      return $ RouteInfo Nothing Nothing bound [] []
    else do
      when (length route.legs > 1) $
        logTagWarning "MMIRoutes" ("More than one element in route.legs, " <> show req)
      let points = PP.decode route.geometry
          bounds = boundingBoxCal points
          boundBox = Just $ BoundingBoxWithoutCRSXY (PointXY bounds.minLat bounds.minLon) (PointXY bounds.minLat bounds.minLon)
          snappedWayPoints = (\waypoint -> waypoint.location.getLatLong) <$> resp.waypoints
          distanceInM = Just $ Meters $ double2Int route.distance
          durationInS = Just $ Seconds $ double2Int route.duration
      return $ RouteInfo durationInS distanceInM boundBox snappedWayPoints points
  where
    createAcc = Acc {minLat = 91.0, maxLat = -91.0, minLon = 180.0, maxLon = -180.0}
    boundingBoxCal points = foldl' compareLatLong createAcc points
    compareLatLong :: Acc -> LatLong -> Acc
    compareLatLong acc loc =
      let max_lat = max loc.lat acc.maxLat
          max_lng = max loc.lon acc.maxLon
          min_lat = min loc.lat acc.minLat
          min_lng = min loc.lon acc.minLon
       in Acc {minLat = min_lat, maxLat = max_lat, minLon = min_lng, maxLon = max_lng}

data Acc = Acc {minLat :: Double, maxLat :: Double, minLon :: Double, maxLon :: Double}
  deriving (Generic, ToJSON, FromJSON)
