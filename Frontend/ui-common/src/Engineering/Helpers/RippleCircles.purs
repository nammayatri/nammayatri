module Engineering.Helpers.RippleCircles where

import Prelude

import Effect (Effect)
import Common.Types.App (CircleRippleConfig, GroundOverlayConfig, MarkerLabelConfig, Paths)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Engineering.Helpers.Commons (callbackMapper)
import Data.Function.Uncurried (Fn3, runFn2)
import JBridge (removeMarker,getLocationNameV2 )


foreign import addRippleCircle :: EffectFn1 CircleRippleConfig Unit
foreign import animateRippleCircle :: EffectFn1 CircleRippleConfig Unit
foreign import removeRippleCircle :: EffectFn1 CircleRippleConfig Unit
foreign import updateRippleCirclePosition :: EffectFn1 CircleRippleConfig Unit
foreign import addGroundOverlay :: EffectFn1 GroundOverlayConfig Unit
foreign import updateGroundOverlay :: EffectFn1 GroundOverlayConfig Unit
foreign import removeGroundOverlay :: EffectFn1 GroundOverlayConfig Unit
foreign import upsertMarkerLabel :: EffectFn1 MarkerLabelConfig Unit
foreign import clearMap :: EffectFn1 String Unit
foreign import isOverlayPresent :: String -> Boolean
foreign import isCirclePresent :: String -> Boolean


addAndUpdateRideSafeOverlay :: Paths -> Effect Unit
addAndUpdateRideSafeOverlay center = 
    case isOverlayPresent "SOSDangerOverlay" of
      true -> do
        removeSOSRipples ""
        addOverlay
      false -> 
        case isOverlayPresent "SOSSafeOverlay" of
          true -> do
            removeSOSRipples ""
            updateSafeGroundOverlay center
          false -> addOverlay
  where
    addOverlay = void $ runEffectFn1 addGroundOverlay groundOverlayConfig{center = center, id = "SOSSafeOverlay", imageUrl = "ny_ic_sos_safe"}

addAndUpdateSOSRipples :: Paths -> Effect Unit
addAndUpdateSOSRipples center = do
  case isOverlayPresent "SOSSafeOverlay" of
    true -> do 
      void $ runEffectFn1 removeGroundOverlay groundOverlayConfig{id = "SOSSafeOverlay"}
      addRipples
    false -> addRipples
  where 
    addRipples = do
      case isCirclePresent "ripples_1" of
        true -> do
          void $ runEffectFn1 updateRippleCirclePosition circleRippleConfig{center = center}
          void $ runEffectFn1 updateGroundOverlay groundOverlayConfig{center = center, id = "SOSDangerOverlay"} 
        false ->  do
          void $ runEffectFn1 addRippleCircle circleRippleConfig{center = center}
          void $ runEffectFn1 animateRippleCircle circleRippleConfig{center = center}
          void $ runEffectFn1 addGroundOverlay groundOverlayConfig{center = center, id = "SOSDangerOverlay"}

addNavigateMarker :: forall action. Paths -> (action -> Effect Unit) -> action -> Effect Unit
addNavigateMarker point push action = do
  let
    callBack = callbackMapper (push action)
    locationName = runFn2 getLocationNameV2 point.lat point.lng
  runEffectFn1 
    upsertMarkerLabel 
      { id: "SOSMarkerLabel"
      , title: locationName
      , actionImage: "ny_ic_navigate"
      , actionCallBack: callBack
      , position: point
      , markerImage : ""
      }
updateSafeGroundOverlay :: Paths -> Effect Unit
updateSafeGroundOverlay center = do
  void $ runEffectFn1 updateGroundOverlay groundOverlayConfig{center = center, id = "SOSSafeOverlay"}
  -- void $ addMarker "ny_ic_green_circle" center.lat center.lng 60 0.5 0.0 -- Need to check with designer not looking good

removeSOSRipples :: String -> Effect Unit
removeSOSRipples _ = do
  void $ runEffectFn1 removeRippleCircle circleRippleConfig
  void $ runEffectFn1 removeGroundOverlay groundOverlayConfig{id = "SOSDangerOverlay"}
  void $ pure $ removeMarker "ny_ic_red_circle"

circleRippleConfig :: CircleRippleConfig
circleRippleConfig = {
  delay : 100
, duration : 1000
, pause : 100
, repeatMode : -1
, count : 5
, radius : 10.0
, maxRadius : 5.0
, strokeWidth : 5.0
, maxStrokeWidth : 8.0
, fromStrokeColor : "#f9cdcc"
, toStrokeColor : "#FF0000"
, prefix : "ripples"
, center : {
  lat : 0.0
, lng : 0.0
}
}

groundOverlayConfig :: GroundOverlayConfig
groundOverlayConfig = {
  id : "<unique>"
, height : 150
, width : 150
, imageUrl : "ny_ic_sos_active"
, fetchFromView : false
, viewId : ""
, center : {
  lat : 0.0
, lng : 0.0
}
}

-- RepeatMode Mappings
-- RESTART = 1
-- REVERSE = 2
-- INFINITE = -1
