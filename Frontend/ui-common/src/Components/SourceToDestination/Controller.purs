{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SourceToDestination.Controller where

import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

data Action = Dummy
type Config = 
  {
    margin :: Margin
  , width :: Length
  , sourceMargin :: Margin
  , sourceBackground :: String
  , sourceImageConfig :: ImageConfig
  , sourceTextConfig :: TextConfig
  , destinationMargin :: Margin
  , destinationBackground :: String
  , destinationImageConfig :: ImageConfig
  , destinationTextConfig :: TextConfig
  , rideStartedAtConfig :: TimeConfig
  , rideEndedAtConfig :: TimeConfig
  , lineMargin :: Margin
  , distanceConfig :: DistanceConfig
  }

type DistanceConfig = {
    distanceVisibility :: Visibility
  , distanceValue :: String
  , background :: String
  , margin :: Margin
}

type ImageConfig =
  {
    width :: Length
  , height :: Length
  , imageUrl :: String
  , margin :: Margin
  }

type TextConfig = 
  {
    text :: String
  , textSize :: Int
  , padding :: Padding
  , margin :: Margin
  , fontStyle :: String
  , color :: String
  , ellipsize :: Boolean
  , maxLines :: Int
  }

type TimeConfig =
  {
    text :: String
  , textSize :: Int
  , fontStyle :: String 
  , color :: String
  , visibility :: Visibility
  , margin :: Margin 
  , padding :: Padding
  , maxLines :: Int 
  , ellipsize :: Boolean 
  }
config :: Config
config = {
    margin : (Margin 0 0 0 0)
  , width : MATCH_PARENT
  , lineMargin : (Margin 5 4 0 0)
  , sourceMargin : (Margin 0 0 0 25)
  , sourceBackground : Color.white900
  , sourceImageConfig : {
      width : V 10
    , height : V 10
    , imageUrl : ""
    , margin : (MarginTop 3)
    }
  , sourceTextConfig : {
      text : ""
    , textSize : FontSize.a_12
    , padding : ( Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.greyDavy 
    , ellipsize : false  
    , maxLines : 10
  }
  , rideStartedAtConfig : {
      text : ""
    , textSize : FontSize.a_12
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.black700
    , visibility : GONE 
    , margin : (MarginTop 2)
    , padding : (Padding 0 0 0 0)
    , maxLines : 1
    , ellipsize : true
    }
  , destinationMargin : (Margin 0 0 0 0)
  , destinationBackground : Color.white900
  , destinationImageConfig : {
      width : V 10
    , height : V 10
    , imageUrl : ""
    , margin : (MarginTop 3)
    }
  , destinationTextConfig : {
      text : ""
    , textSize : FontSize.a_12
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.greyDavy
    , ellipsize : false
    , maxLines : 10
    }
  , rideEndedAtConfig : {
      text : ""
    , textSize : FontSize.a_12
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.black700
    , visibility : GONE 
    , margin : (MarginTop 2)
    , maxLines : 1
    , ellipsize : false
    , padding : (Padding 0 0 0 0)
    }
  , distanceConfig : {
      distanceVisibility : GONE
    , distanceValue : ""
    , background : ""
    , margin : Margin 10 27 0 0
  }
}