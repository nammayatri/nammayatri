{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SourceToDestination.View where

import Prelude (Unit, ($), (<>), (/), (<), (>), (==),(/=), const, map)
import Effect (Effect)
import Components.SourceToDestination.Controller (Action(..), Config, PillInfo)
import PrestoDOM 
import Common.Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Common.Types.App (LazyCheck(..))
import Components.SeparatorView.View as SeparatorView
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import Constants (defaultSeparatorCount, getSeparatorFactor)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Function.Uncurried (runFn1)
import JBridge (getLayoutBounds)
import Mobility.Prelude (boolToVisibility)
import Debug
import PrestoDOM.Animation as PrestoAnim
import Animation (scaleYAnimWithDelay)

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  PrestoAnim.animationSet
  [scaleYAnimWithDelay 10] $
  frameLayout
  [ height WRAP_CONTENT
  , width config.width
  , gravity CENTER_VERTICAL
  , margin config.margin
  -- , onAnimationEnd push $ const NoAction
  ][ relativeLayout
      ([ height WRAP_CONTENT
      , width MATCH_PARENT
      ] <> case config.id of
        Just layoutId -> [id $ getNewIDWithTag $ "src_dest_layout_" <> layoutId]
        Nothing -> [])((if config.destinationTextConfig.text /= "" then [ 
        linearLayout
        [ height WRAP_CONTENT
        , orientation VERTICAL
        , width MATCH_PARENT
        , margin $ MarginTop config.separatorMargin
        , visibility $ boolToVisibility config.showDestination
        ][SeparatorView.view $ separatorConfig config
        , destinationLayout config push]
      ]
       else []) <>
      [
      sourceLayout push config
      ])
    , distanceLayout config
    ]


sourceLayout :: forall w. (Action  -> Effect Unit)  -> Config -> PrestoDOM (Effect Unit) w
sourceLayout push config =
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , margin config.sourceMargin
    , accessibility DISABLE
    -- , afterRender push $ const NoAction
    ][  imageView
        [ width config.sourceImageConfig.width
        , height config.sourceImageConfig.height
        , accessibility DISABLE
        , imageWithFallback config.sourceImageConfig.imageUrl
        , margin config.sourceImageConfig.margin
        ]
      , linearLayout
        ([ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , accessibility ENABLE
        , accessibilityHint $ "PickUp Location Is " <> config.sourceTextConfig.text
        , gravity CENTER_VERTICAL
        ] <> case config.id of
          Just layoutId -> [id $ getNewIDWithTag $ "source_layout_" <> layoutId]
          Nothing -> [])
          $ [  textView $
            [ text config.sourceTextConfig.text
            , width MATCH_PARENT
            , accessibility DISABLE
            , padding config.sourceTextConfig.padding
            , margin config.sourceTextConfig.margin
            , color config.sourceTextConfig.color
            , ellipsize config.sourceTextConfig.ellipsize
            , maxLines config.sourceTextConfig.maxLines
            ] <> (FontStyle.getFontStyle config.sourceTextConfig.textStyle LanguageStyle)
          , textView $
            [ text config.rideStartedAtConfig.text
            , color config.rideStartedAtConfig.color
            , accessibility DISABLE
            , visibility config.rideStartedAtConfig.visibility
            , margin config.rideStartedAtConfig.margin
            , padding config.rideStartedAtConfig.padding
            , maxLines config.rideStartedAtConfig.maxLines
            , ellipsize config.rideStartedAtConfig.ellipsize
            ] <> (FontStyle.getFontStyle config.rideStartedAtConfig.textStyle LanguageStyle)
          , linearLayout
              [ height $ config.horizontalSeperatorConfig.height
              , width $ config.horizontalSeperatorConfig.width
              , margin $ config.horizontalSeperatorConfig.margin
              , background $ config.horizontalSeperatorConfig.background
              , visibility $ config.horizontalSeperatorConfig.visibility
              , padding $ config.horizontalSeperatorConfig.padding
              ][]
          , horizontalScrollView
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , margin $ MarginLeft 8
              -- , background $ config.horizontalSeperatorConfig.background
              , visibility $ config.pillsConfig.visibility
              , orientation HORIZONTAL
              -- , padding $ config.horizontalSeperatorConfig.padding
              ]
              [ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , margin $ MarginVertical 16 16
                  , scrollBarX false
                  ]
                  ( map (\item -> mapPill config push item) config.pillsConfig.pillList)
                  -- [ mapPill config push
                  -- , mapPill config push 
                  -- -- , mapPill config push
                  -- -- , mapPill config push
                  -- -- , mapPill config push
                  -- -- , mapPill config push
                  -- ]
              ]
          ]
      ]


mapPill :: forall w. Config -> (Action -> Effect Unit) -> PillInfo -> PrestoDOM (Effect Unit) w
mapPill state push pill = 
    linearLayout 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , background Color.grey700
        , stroke ("1," <> Color.grey900)
        , padding $ Padding 8 6 8 6
        , cornerRadius 4.0
        , margin $ MarginHorizontal 4 4
        ]
        [ imageView
            [ width $ V 16
            , height $ V 16
            , imageWithFallback pill.imageUrl
            , visibility pill.imageVisibility
            , margin $ MarginRight 4
            , gravity CENTER
            ]
        , textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text pill.text
            , color Color.black800
            , gravity CENTER
            ] <> FontStyle.tags TypoGraphy
        ]
    


destinationLayout :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationLayout config push =
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , margin config.destinationMargin
  , visibility $ boolToVisibility config.showDestination
  ][  linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , margin config.destinationImageConfig.margin
      , background config.destinationBackground
      ][  imageView
          [ width config.destinationImageConfig.width
          , height config.destinationImageConfig.height
          , imageWithFallback config.destinationImageConfig.imageUrl
          ]
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      , accessibilityHint $ "Drop Location is : " <> config.destinationTextConfig.text
      , accessibility ENABLE
      ][  textView $
          [ text config.destinationTextConfig.text
          , layoutGravity "center_vertical"
          , padding config.destinationTextConfig.padding
          , width MATCH_PARENT
          , margin config.destinationTextConfig.margin
          , color config.destinationTextConfig.color
          , maxLines config.destinationTextConfig.maxLines
          , accessibility DISABLE
          , clickable config.destinationTextConfig.isClickable
          , ellipsize config.destinationTextConfig.ellipsize
          , onClick push $ const DestinationClicked 
          ] <> (FontStyle.getFontStyle config.destinationTextConfig.textStyle LanguageStyle)
        , textView $
          [ text config.rideEndedAtConfig.text
          , color config.rideEndedAtConfig.color
          , visibility config.rideEndedAtConfig.visibility
          , margin config.rideEndedAtConfig.margin
          , padding config.rideEndedAtConfig.padding
          , maxLines config.rideEndedAtConfig.maxLines
          , accessibility DISABLE
          , ellipsize config.rideEndedAtConfig.ellipsize
          ] <> (FontStyle.getFontStyle config.rideEndedAtConfig.textStyle LanguageStyle)
        ]
    ]

distanceLayout :: forall w. Config -> PrestoDOM (Effect Unit) w
distanceLayout config =
  linearLayout
  [ width WRAP_CONTENT
  , height $ getDistanceLayoutHeight config
  , gravity CENTER_VERTICAL
  ]
  [linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , stroke $ "1," <> Color.grey900
  , cornerRadius 4.0
  , margin config.distanceConfig.margin
  , background config.distanceConfig.background
  , visibility config.distanceConfig.distanceVisibility
  ][ textView $
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , text config.distanceConfig.distanceValue
      , accessibility if config.distanceConfig.distanceVisibility == VISIBLE then ENABLE else DISABLE
      , accessibilityHint $ "Distance Between PickUp And Destination Location : " <> config.distanceConfig.distanceValue
      , color Color.black900
      , padding $ Padding 6 4 6 4
      ] <> FontStyle.tags LanguageStyle
  ]]

separatorConfig :: Config -> SeparatorView.Config
separatorConfig config = 
  let count = case config.id of 
                Nothing -> defaultSeparatorCount  
                Just layoutId -> (runFn1 getLayoutBounds $ getNewIDWithTag $ "source_layout_" <> layoutId).height / getSeparatorFactor
  in {
    orientation : VERTICAL
  , count : if config.overrideSeparatorCount > 0 then config.overrideSeparatorCount else if count < defaultSeparatorCount then defaultSeparatorCount else count
  , height : V 4
  , width : V 2
  , layoutWidth : config.destinationImageConfig.width
  , layoutHeight : config.destinationImageConfig.height
  , color : Color.black500
  }

getDistanceLayoutHeight :: Config -> Length
getDistanceLayoutHeight config = if os == "ANDROID" then MATCH_PARENT else 
  case config.id of
    Nothing -> MATCH_PARENT
    Just layoutId -> V $ (runFn1 getLayoutBounds $ getNewIDWithTag $ "src_dest_layout_" <> layoutId).height