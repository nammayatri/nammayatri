{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketingScreen.View where

import Prelude
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.SettingSideBar as SettingSideBar
import Data.Array (mapWithIndex)
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Engineering.Helpers.Commons (flowRunner)
import Effect.Class (liftEffect)
import Data.Maybe (fromMaybe, Maybe(..))
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import PrestoDOM (lineHeight, Gravity(..), Length(..), Margin(..), maxLines, ellipsize, Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, linearLayout, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, shimmerFrameLayout, imageUrl, alignParentBottom)
import Screens.TicketingScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.Types as ST
import Styles.Colors as Color
import Services.API as API
import Services.Backend as Remote
import Data.Array as DA
import Font.Size as FontSize
import Mobility.Prelude

screen :: ST.TicketingScreenState -> Screen Action ST.TicketingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "TicketingScreen"
  , globalEvents: [getPlaceDataEvent]
  , eval
  }
  where 
  getPlaceDataEvent push = do
    void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ getPlaceDataEvent' push
    pure $ pure unit
  
  getPlaceDataEvent' push = do
    placesResp <- Remote.getTicketPlacesBT ""
    lift $ lift $ doAff do liftEffect $ push $ UpdatePlacesData placesResp

view :: forall w. (Action -> Effect Unit) -> ST.TicketingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
     relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.white900
        , onBackPressed push $ const BackPressed
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , background Color.white900
            ]
            [ headerView push state
            , linearLayout
                [ weight 1.0
                , width MATCH_PARENT
                , gravity CENTER
                ]
                [ ticketingList push state ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity BOTTOM
                , weight 1.0
                ]
                []
            ]
        ]

headerView :: forall w. (Action -> Effect Unit) -> ST.TicketingScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.black900
    , orientation VERTICAL
    , padding $ Padding 16 20 16 25
    ]
    [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , onClick push $ const $ BackPressed
        ]
        [ imageView
            [ width $ V 20
            , height $ V 20
            , imageWithFallback $ fetchImage FF_ASSET "ic_chevron_left_white"
            ]
        , imageView
            [ width $ V 78
            , height $ V 42
            , margin $ MarginLeft 5
            , imageWithFallback $ fetchImage FF_ASSET "ny_ys_logo_text"
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 29
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ text "Yatri Sathi Ticketing"
              , color Color.white900
              , weight 1.0
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              ]
            <> FontStyle.subHeading1 LanguageStyle
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , cornerRadius 30.0
            , padding $ Padding 8 8 8 8
            , gravity CENTER_VERTICAL
            , background Color.black700
            , onClick push $ const $ MyTicketsAC
            , visibility $ boolToVisibility (not state.props.hideMyTickets)
            ]
            [ textView
                $ [ text "My Tickets"
                  , color Color.white900
                  , height WRAP_CONTENT
                  , padding $ Padding 3 0 0 3
                  ]
                <> FontStyle.paragraphText LanguageStyle
            , imageView
                [ width $ V 16
                , height $ V 16
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
                ]
            ]
        ]
    ]

ticketingList :: forall w. (Action -> Effect Unit) -> ST.TicketingScreenState -> PrestoDOM (Effect Unit) w
ticketingList push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER_HORIZONTAL
    , margin $ MarginTop 16
    ] if DA.null state.data.placeInfoArray then [sfl 70, sfl 70] else
    ( mapWithIndex
        (\index item -> ticketingItem push item index)
        state.data.placeInfoArray
    )

ticketingItem :: forall w. (Action -> Effect Unit) -> API.TicketPlaceResp -> Int -> PrestoDOM (Effect Unit) w
ticketingItem push (API.TicketPlaceResp item) index =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 16.0
  , background Color.yellow800
  , margin $ Margin 16 marginTop 16 0
  , gravity CENTER
  ] $ [  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , background Color.white900
          , cornerRadius 16.0
          , stroke $ "1," <> Color.grey900
          , padding $ Padding 12 12 12 12
          , onClick push $ const $ OnSelect $ API.TicketPlaceResp item
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , weight 1.0
              , gravity CENTER_VERTICAL
              ]
              [  linearLayout
                  [ width $ WRAP_CONTENT
                  , height $ WRAP_CONTENT
                  , cornerRadius 6.0
                  ][  imageView
                      [ imageUrl $ fromMaybe "ny_ic_jetty" item.iconUrl -- TODO: Get default image
                      , cornerRadius 6.0
                      , height $ V 64
                      , width $ V 64
                  ]
                ]
              , linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , weight 1.0
                  , orientation VERTICAL
                  , margin $ MarginLeft 16
                  ]
                  [ textView
                      $ [ text $ item.name
                        , color Color.black800
                        , height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , padding $ PaddingBottom 2
                        , maxLines 2
                        , lineHeight "15"
                        , ellipsize true
                        , fontStyle $ FontStyle.bold LanguageStyle
                        , textSize $ FontSize.a_18
                        ]
                  , textView
                      $ [ color Color.black700
                        , height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , padding $ PaddingBottom 2
                        , maxLines 2
                        , ellipsize true
                        , lineHeight "17"
                        , textSize FontSize.a_14
                        , fontStyle $ FontStyle.regular LanguageStyle
                        ] <> case item.shortDesc of
                          Nothing -> [visibility GONE]
                          Just desc -> [text $ desc]
                  ]
          ]
      ,   linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity RIGHT
          ]
          [ imageView
              [ width $ V 32
              , height $ V 32
              , cornerRadius 32.0
              , margin $ MarginTop 8
              , padding $ Padding 8 8 8 8
              , imageWithFallback $ fetchImage FF_ASSET "ny_ic_right_arrow_blue"
              , background Color.blue100
              ]
          ]
      ]
    ] 
  where
  marginTop = if index == 0 then 0 else 16

hurryLimitedView :: forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
hurryLimitedView push =
  textView $
  [ text "Hurry! Limited number of tickets available today! adsfkjaksdfjaksjd fkafjdk;a .jdfkl;ajdkfja; klj"
  , color Color.black900
  , width WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginVertical 8 8 
  ] <> FontStyle.tags TypoGraphy

sfl :: forall w. Int -> PrestoDOM (Effect Unit) w
sfl height' = 
  shimmerFrameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , cornerRadius 8.0
    , padding $ Padding 15 15 15 15
    , stroke $ "2," <> Color.grey900
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height $ V height'
        , background Color.grey900
        , cornerRadius 8.0
        ][]
    ]