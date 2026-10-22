{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SourceToDestination.View where

import Prelude (Unit, ($), (<>), (/), (<), (>), (==),(/=), const, map, (-), unit, (+))
import Effect (Effect)
import Components.SourceToDestination.Controller (Action(..), Config, PillInfo)
import PrestoDOM 
import Common.Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Common.Types.App (LazyCheck(..))
import Components.SeparatorView.View as SeparatorView
import Engineering.Helpers.Commons (getNewIDWithTag, os, screenWidth)
import Constants (defaultSeparatorCount, getSeparatorFactor)
import Data.Array ((!!), length, mapWithIndex)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Function.Uncurried (runFn1)
import Data.String as DS
import JBridge (getLayoutBounds)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM.Animation as PrestoAnim
import Animation (scaleYAnimWithDelay)

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  if config.showSourceDestWithStops then viewWithStops push config
  else
  frameLayout
  [ height WRAP_CONTENT
  , width config.width
  , gravity CENTER_VERTICAL
  , margin config.margin
  , afterRender push $ const AfterRender
  ][ relativeLayout
      ([ height WRAP_CONTENT
      , width MATCH_PARENT
      -- , orientation VERTICAL
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


viewWithStops :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
viewWithStops push config =
  frameLayout
    [ height WRAP_CONTENT
    , width config.width
    , gravity CENTER_VERTICAL
    , margin config.margin
    , afterRender push $ const AfterRender
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ relativeLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            ]
            [ linearLayout[
              height WRAP_CONTENT
              , width WRAP_CONTENT
              , margin config.separatorLayoutMargin
              ][
              SeparatorView.view $ (separatorConfig config)]
            , linearLayout
                [ height WRAP_CONTENT
                , orientation VERTICAL
                , width MATCH_PARENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , orientation VERTICAL
                    , width MATCH_PARENT
                    , id $ getNewIDWithTag $ "source_destiniation_view" <> (fromMaybe "" config.id)
                    , visibility $ boolToVisibility config.showDestination
                    ]
                    [ sourceLayout push config
                    , stopsView config push
                    ]
                , destinationLayout config push
                ]
            ]
        ]
    , distanceLayout config
    ]

sourceLayout :: forall w. (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceLayout push config =
  let dateAndAddress = addressAndDate config.sourceTextConfig.text
      address = dateAndAddress.address
      date = dateAndAddress.date
  in
  linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , margin config.sourceMargin
    , accessibility DISABLE
    , afterRender push $ const AfterRender
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
        , accessibilityHint $ (if date /= "" then "Ride started at : " <> date <> " from " else "") <> "PickUp Location : " <> address
        , gravity CENTER_VERTICAL
        ] <> case config.id of
          Just layoutId -> [id $ getNewIDWithTag $ "source_layout_" <> layoutId]
          Nothing -> [])
          $ [  textView $
            [ text config.sourceTextConfig.text
            , width $ V $ (screenWidth unit) - 20
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
              , margin $ MarginLeft 4
              , visibility $ config.pillsConfig.visibility
              , orientation HORIZONTAL
              ]
              [ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , margin $ MarginVertical 16 16
                  , scrollBarX false
                  ]
                  ( map (\item -> mapPill config push item) config.pillsConfig.pillList)
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
    , gravity CENTER_VERTICAL
    ]
    [ imageView
        [ width $ V 16
        , height $ V 16
        , imageWithFallback pill.imageUrl
        , visibility pill.imageVisibility
        , margin $ MarginRight 2
        , gravity CENTER_VERTICAL
        ]
    , textView $ 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text pill.text
        , color Color.black800
        , gravity CENTER_VERTICAL
        ] <> FontStyle.tags TypoGraphy
    ]
    
addressAndDate :: String -> {address :: String, date :: String}
addressAndDate text = do
  let dateAndAddress = DS.split(DS.Pattern ("\n")) text
      address = if length dateAndAddress > 1 then fromMaybe "" (dateAndAddress !! 1) else fromMaybe "" (dateAndAddress !! 0)
      date = if length dateAndAddress > 1 then fromMaybe "" (dateAndAddress !! 0) else ""
  {address: address, date: date}

destinationLayout :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationLayout config push =
  let dateAndAddress = addressAndDate config.destinationTextConfig.text
      address = dateAndAddress.address
      date = dateAndAddress.date
  in
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
      , accessibilityHint $ (if date /= "" then "Trip ended at : " <> date <> " on " else "") <> "Drop Location : " <> address
      , accessibility ENABLE
      ][  textView $
          [ text config.destinationTextConfig.text
          , layoutGravity "center_vertical"
          , padding config.destinationTextConfig.padding
          , width $ V $ (screenWidth unit) - 20
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
                Just layoutId -> (runFn1 getLayoutBounds $ getNewIDWithTag $ "source_destiniation_view" <> layoutId).height / getSeparatorFactor
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
getDistanceLayoutHeight config = 
  if config.showSourceDestWithStops then V $ (runFn1 getLayoutBounds $ getNewIDWithTag "all_stops_view").height
  else if os == "ANDROID" then MATCH_PARENT else 
    case config.id of
      Nothing -> MATCH_PARENT
      Just layoutId -> V $ (runFn1 getLayoutBounds $ getNewIDWithTag $ "src_dest_layout_" <> layoutId).height


stopsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
stopsView config push =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ MarginBottom 16
    , orientation VERTICAL
    ]
    (mapWithIndex (\index item -> stopView item) (config.stops))
  where
  stopView name =
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , margin $ MarginTop 16
      ]
      [ imageView
          [ width config.stopsImageConfig.width
          , height config.stopsImageConfig.height
          , accessibility DISABLE
          , imageWithFallback config.stopsImageConfig.imageUrl
          , margin config.stopsImageConfig.margin
          ]
      , textView
          $ [ text name
            , margin $ MarginLeft 16
            , color Color.greyDavy
            ]
          <> FontStyle.body3 LanguageStyle
      ]
