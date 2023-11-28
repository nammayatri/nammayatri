module Components.SeparatorView.View where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import JBridge (getArray)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, background, cornerRadius, gravity, height, linearLayout, margin, orientation, width)
import Styles.Colors as Color


view :: Config -> forall w . PrestoDOM (Effect Unit) w 
view config = 
  let viewConfig = getConfigByOrientation config.orientation config
  in linearLayout
  [ height viewConfig.height
  , width  viewConfig.width
  , gravity CENTER
  , margin $ MarginVertical 2 2
  , orientation config.orientation
  ](map (\_ -> linearLayout[height config.height
  , width config.width
  , background config.color
  , cornerRadius $ getCornerRadius config.width
  , margin viewConfig.margin
  ][]) (getArray config.count))



getConfigByOrientation :: Orientation -> Config -> {margin :: Margin, height :: Length, width :: Length}
getConfigByOrientation orientation config = case orientation of
  VERTICAL -> {
    margin : MarginVertical 2 2
  , height : WRAP_CONTENT
  , width : config.layoutWidth
  }
  HORIZONTAL -> {
    margin : MarginHorizontal 2 2
  , height : config.layoutHeight
  , width : WRAP_CONTENT
  }



type Config = {
  orientation :: Orientation
, count :: Int
, height :: Length 
, width :: Length
, layoutWidth :: Length
, layoutHeight :: Length
, color :: String
}

getCornerRadius :: Length -> Number
getCornerRadius len = let 
  width = case len of 
    V n -> n
    _ -> 2
  in toNumber $ width / 2