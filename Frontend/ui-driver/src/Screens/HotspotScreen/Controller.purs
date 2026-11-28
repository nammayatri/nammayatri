{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HotspotScreen.Controller where

import Common.Resources.Constants
import Components.PrimaryButton as PrimaryButtonController
import Data.Array (mapWithIndex, filter, (..), foldl, sortBy, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (fromString, pi, cos, sin)
import Data.Ord (comparing)
import Effect (Effect)
import Effect.Uncurried(runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (getColorWithOpacity)
import Helpers.Utils (toRad)
import JBridge as JB
import MerchantConfig.Types
import Prelude (pure, unit, class Show, bind, discard, ($), void, (<>), show, (>=), (<=), (&&), map, (/), (*), max, (>), Unit, (<), negate, (+), (||), (/=), when)
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.HomeScreen.Controller (getLastKnownLocValue)
import Screens.Types as ST
import Services.API (LatLong(..), DemandHotspotsResp(..), HotspotsDetails(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color

instance showAction :: Show Action where
  show (ShowMap _ _ _) = "ShowMap"
  show (AfterRender ) = "AfterRender"
  show (BackPressed ) = "BackPressed"
  show (BackButtonClicked ) = "BackButtonClicked"
  show (UpdateHotspotCircles _ _ _ _) = "UpdateHotspotCircles"
  show (CircleOnClick _ _ _) = "CircleOnClick"
  show (RefreshHotspots _ _) = "RefreshHotspots"
  show (DemandHotspotApiResponseAction _) = "DemandHotspotApiResponseAction"
  show (UpdateRefreshAnimation ) = "UpdateRefreshAnimation"
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1
  show (NoAction ) = "NoAction"

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = ShowMap String String String
            | AfterRender
            | BackPressed
            | BackButtonClicked
            | UpdateHotspotCircles String String String String
            | CircleOnClick String String String
            | RefreshHotspots String String
            | DemandHotspotApiResponseAction DemandHotspotsResp
            | UpdateRefreshAnimation 
            | PrimaryButtonAC PrimaryButtonController.Action
            | NoAction

data ScreenOutput = GoBack 
                  | RefreshingHotspots ST.HotspotScreenState

eval :: Action -> ST.HotspotScreenState -> Eval Action ScreenOutput ST.HotspotScreenState

eval (DemandHotspotApiResponseAction (DemandHotspotsResp resp)) state = do
  let pointsWithWeight = mapWithIndex (\index (HotspotsDetails item) -> {latlong : fixedDistanceLatLonWithRandomAngle item.location state.data.config.hotspotConfig.centerDeviation, weight : (toNumber item.frequency), id : "hotspot_" <> show index}) resp.hotspotsDetails
  void $ pure $ JB.removeAllPolylines ""
  continueWithCmd state { data {pointsWithWeight = pointsWithWeight, dataExpiryAt = resp.expiryAt}, props {refreshAnimation = false}} [do
    when (state.data.currentDriverLat /= 0.0 && state.data.currentDriverLon /= 0.0) $ do void $ JB.animateCamera state.data.currentDriverLat state.data.currentDriverLon 14.0 "ZOOM"
    pure $ UpdateHotspotCircles state.props.mapCorners.leftPoint state.props.mapCorners.rightPoint state.props.mapCorners.topPoint state.props.mapCorners.bottomPoint
  ]

eval (UpdateHotspotCircles leftPoint rightPoint topPoint bottomPoint) state = 
  if state.props.isAnyCircleSelected then continue state {props {isAnyCircleSelected = false, mapCorners = {leftPoint : leftPoint, rightPoint : rightPoint, topPoint : topPoint, bottomPoint : bottomPoint}}}
  else continueWithCmd state {props {showNavigationSheet = false}} [do
    let left = fromMaybe 0.0 $ fromString leftPoint
        right = fromMaybe 0.0 $ fromString rightPoint
        top = fromMaybe 0.0 $ fromString topPoint
        bottom = fromMaybe 0.0 $ fromString bottomPoint
        hotspotConfig = state.data.config.hotspotConfig
        circlesWithinMap = 
          filter
            (\item -> do
              let (LatLong coordinate) = item.latlong
              (coordinate.lat >= left && coordinate.lat <= right && coordinate.lon >= bottom && coordinate.lon <= top)
            ) state.data.pointsWithWeight
        circlesWithinMapLength = length circlesWithinMap
        circlesWithinMapWithRelativeWeights = if (hotspotConfig.showColorWithRelativeWeight || circlesWithinMapLength < hotspotConfig.minCirclesNeededForSortedWeights) then getRelativeWeights circlesWithinMap else getCirclesSortedWithWeight circlesWithinMap
    void $ foldMapWithIndex 
            (\index item -> do 
                let (LatLong coordinate) = item.latlong
                    circleColor = getCircleColor item hotspotConfig (index + 1) circlesWithinMapLength
                void $ runEffectFn1 JB.drawCircleOnMap JB.defaultCircleConfig { radius = hotspotConfig.circleRadius, primaryStrokeColor = circleColor , fillColor = getColorWithOpacity 23 circleColor, strokeWidth = 4, secondaryStrokeColor = "#00FFFFFF", circleId = item.id, centerLat = coordinate.lat, centerLon = coordinate.lon, strokePattern = show JB.DASHED, isCircleClickable = true }
            ) circlesWithinMapWithRelativeWeights
    pure AfterRender
  ]

eval (ShowMap key lat lon) state = continueWithCmd state{data{ currentDriverLat = getLastKnownLocValue ST.LATITUDE lat, currentDriverLon = getLastKnownLocValue ST.LONGITUDE lon}, props{lastUpdatedTime = EHC.getCurrentUTC ""}} [ do
  void $ JB.getCurrentPosition (showDriverMarker  true) constructLatLong
  void $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = true, lat = fromMaybe 0.0 (fromString lat), lon = fromMaybe 0.0 (fromString lon), zoomLevel = 14.0 }
  pure AfterRender 
  ]

eval BackPressed state =
  if state.props.showNavigationSheet then continue state {props{showNavigationSheet = false}}
  else exit GoBack

eval BackButtonClicked state = exit GoBack

eval (CircleOnClick lat lon color) state = continue state {props {showNavigationSheet = true, selectedCircleColor = color, selectedCircleLatLng = LatLong { lat : fromMaybe 0.0 $ fromString lat, lon : fromMaybe 0.0 $ fromString lon}, isAnyCircleSelected = true}}

eval (RefreshHotspots lat long) state = do
  let updatedState = state {data{ currentDriverLat = getLastKnownLocValue ST.LATITUDE lat,  currentDriverLon = getLastKnownLocValue ST.LONGITUDE long }, props{refreshAnimation = true, showNavigationSheet = false, isAnyCircleSelected = false}}
  updateAndExit updatedState $ RefreshingHotspots updatedState

eval UpdateRefreshAnimation state =
  continueWithCmd state {props{lastUpdatedTime = EHC.getCurrentUTC "", refreshAnimation = false}} [do
      void $ JB.animateCamera state.data.currentDriverLat state.data.currentDriverLon 14.0 "ZOOM"
      pure NoAction
    ]

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = do
  let (LatLong latLong) = state.props.selectedCircleLatLng
      _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_navigate_to_hotspots_evnt"
  void $ pure $ JB.openNavigation latLong.lat latLong.lon "DRIVE"
  continue state 

eval _ state = update state

getRelativeWeights :: Array ST.PointsWithWeight -> Array ST.PointsWithWeight
getRelativeWeights circlesWithinMap = map (\x -> x { weight = (x.weight * 100.0) / maxValue }) circlesWithinMap
  where
  maxValue = foldl getMaxWeight 0.0 circlesWithinMap

getMaxWeight :: Number -> ST.PointsWithWeight -> Number
getMaxWeight num1 obj1 = max obj1.weight num1

getCirclesSortedWithWeight :: Array ST.PointsWithWeight -> Array ST.PointsWithWeight
getCirclesSortedWithWeight circlesWithinMap = sortBy (comparing _.weight) circlesWithinMap

showDriverMarker :: Boolean -> ST.Location -> Effect Unit
showDriverMarker toAnimateCamera location = do
      void $ pure $ JB.enableMyLocation true
      if toAnimateCamera then
        JB.animateCamera location.lat location.lon 13.0 "ZOOM"
      else pure unit

constructLatLong :: String -> String -> ST.Location
constructLatLong lat lon =
  { lat: fromMaybe 0.0 (fromString lat)
  , lon : fromMaybe 0.0 (fromString lon)
  , place : ""
  , driverInsideThreshold : false
  }

fixedDistanceLatLonWithRandomAngle :: LatLong -> Number -> LatLong
fixedDistanceLatLonWithRandomAngle (LatLong latlong) distance =
  let randomAngle = fromMaybe 0.0 $ fromString $ EHC.getRandomID 360 
      distanceInDegrees = distance / metersPerDegreeLat
      angleInRadians = toRad randomAngle
      newLatitude = latlong.lat + (distanceInDegrees * cos angleInRadians)
      newLongitude = latlong.lon + (distanceInDegrees * sin angleInRadians) / cos (latlong.lat * pi / 180.0)
  in LatLong { lat : newLatitude, lon : newLongitude}

getCircleColor :: ST.PointsWithWeight -> HotspotConfig -> Int -> Int -> String
getCircleColor item hotspotConfig index arrayLength = do
  let rank = if (hotspotConfig.showColorWithRelativeWeight || arrayLength < hotspotConfig.minCirclesNeededForSortedWeights) then item.weight else ((toNumber index) * 100.0)/(toNumber arrayLength)
  if rank < hotspotConfig.highRange then hotspotConfig.moderateHotspotColor else if rank < hotspotConfig.veryHighRange then hotspotConfig.highHotspotColor else hotspotConfig.veryHighHotspotColor