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
import Font.Style (Style(..))
import Common.Styles.Colors as Color
import Common.Types.App
import Data.Maybe (Maybe(..))

data Action = DestinationClicked | AfterRender
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
  , separatorMargin :: Int
  , id :: Maybe String
  , overrideSeparatorCount :: Int
  , horizontalSeperatorConfig :: SeparatorConfig
  , pillsConfig :: PillsConfig
  , showDestination :: Boolean
  , separatorLayoutMargin :: Margin
  , stops :: Array String
  , stopsImageConfig :: ImageConfig
  , showSourceDestWithStops :: Boolean
  }

type PillsConfig = {
    visibility :: Visibility
  , pillList :: Array PillInfo
}

type PillInfo = {
    text :: String
  , imageUrl :: String
  , imageVisibility :: Visibility
}

type SeparatorConfig = {
    visibility :: Visibility
  , margin :: Margin
  , background :: String
  , height :: Length
  , width :: Length
  , padding :: Padding
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
  , padding :: Padding
  , margin :: Margin
  , color :: String
  , ellipsize :: Boolean
  , maxLines :: Int
  , textStyle :: Style
  , isEditable :: Boolean
  , isClickable :: Boolean
  }

type TimeConfig =
  {
    text :: String
  , color :: String
  , visibility :: Visibility
  , margin :: Margin
  , padding :: Padding
  , maxLines :: Int 
  , ellipsize :: Boolean 
  , textStyle :: Style
  }
config :: Config
config = {
    margin : (Margin 0 0 0 0)
  , width : MATCH_PARENT
  , lineMargin : (Margin 5 4 0 0)
  , sourceMargin : (Margin 0 0 0 0)
  , sourceBackground : Color.white900
  , separatorMargin : 15
  , sourceImageConfig : {
      width : V 10
    , height : V 10
    , imageUrl : ""
    , margin : (MarginTop 3)
    }
  , sourceTextConfig : {
      text : ""
    , padding : ( Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , color : Color.greyDavy 
    , ellipsize : false  
    , maxLines : 10
    , textStyle : Body3
    , isEditable : false
    , isClickable :false
  }
  , rideStartedAtConfig : {
      text : ""
    , color : Color.black700
    , visibility : GONE
    , margin : (MarginTop 2)
    , padding : (Padding 0 0 0 0)
    , maxLines : 1
    , ellipsize : true
    , textStyle : Body3
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
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , color : Color.greyDavy
    , ellipsize : false
    , maxLines : 10
    , textStyle : Body3
    , isEditable : false
    , isClickable : false
    }
  , rideEndedAtConfig : {
      text : ""
    , color : Color.black700
    , visibility : GONE
    , margin : (MarginTop 2)
    , maxLines : 1
    , ellipsize : false
    , padding : (Padding 0 0 0 0)
    , textStyle : Body3
    }
  , distanceConfig : {
      distanceVisibility : GONE
    , distanceValue : ""
    , background : ""
    , margin : Margin 0 0 0 0
  },
  horizontalSeperatorConfig : {
      visibility : GONE
    , background : ""
    , margin : Margin 0 0 0 0
    , padding : Padding 0 0 0 0
    , width : MATCH_PARENT
    , height : V 1
  },
  pillsConfig : {
      visibility : GONE 
    , pillList : []
  }
  , id : Nothing
  , overrideSeparatorCount : 0
  , showDestination : true
  , separatorLayoutMargin : Margin 0 0 0 0
  , stops : [] 
  , stopsImageConfig : {
      width : V 10
    , height : V 10
    , imageUrl : ""
    , margin : MarginTop 0
    }
  , showSourceDestWithStops : false
  }
