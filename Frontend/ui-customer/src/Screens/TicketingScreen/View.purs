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
import Screens.TicketingScreen.ComponentConfig

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
import Effect (Effect)
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, linearLayout, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import Screens.TicketingScreen.Controller (Action(..), eval, ScreenOutput(..))
import Screens.Types as ST
import Styles.Colors as Color
import Components.NavBar as NavBar
import Services.API as API
import Services.Backend as Remote
import Data.Array as DA

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
  -- Anim.screenAnimation $
     relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
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
                [ NavBar.view (push <<< NavBarAC) 1 ]
            ]
          , settingSideBarView push state
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
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ width $ V 20
            , height $ V 20
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_hamburger_white"
            , onClick push $ const $ HamburgerClick
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
            ]
            [ textView
                $ [ text "My Tickets"
                  , color Color.white900
                  , height WRAP_CONTENT
                  , padding $ PaddingBottom 3
                  ]
                <> FontStyle.subHeading1 LanguageStyle
            , imageView
                [ width $ V 20
                , height $ V 20
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
    ]
    ( mapWithIndex
        (\index item -> ticketingItem push item index)
        [ { title: "Alipore Zoo", description: "Explore Alipore Zoo", icon: "ny_ic_animal_1" }
        , { title: "Millennium Park Jetty", description: "Book Ferry Tickets on the Hoogli", icon: "ny_ic_jetty" }
        ]
    )

ticketingItem :: forall w. (Action -> Effect Unit) -> { title :: String, description :: String, icon :: String } -> Int -> PrestoDOM (Effect Unit) w
ticketingItem push item index =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , cornerRadius 6.0
    , stroke $ "1," <> Color.grey900
    , margin $ Margin 16 marginTop 16 0
    , padding $ Padding 8 16 12 16
    , onClick push $ const $ OnSelect index
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET item.icon
            , cornerRadius 6.0
            , height $ V 48
            , width $ V 60
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , weight 1.0
            , orientation VERTICAL
            , margin $ MarginLeft 16
            ]
            [ textView
                $ [ text item.title
                  , color Color.black800
                  , height WRAP_CONTENT
                  , padding $ PaddingBottom 2
                  ]
                <> FontStyle.h3 LanguageStyle
            , textView
                $ [ text item.description
                  , color Color.black900
                  , height WRAP_CONTENT
                  , padding $ PaddingBottom 2
                  ]
                <> FontStyle.tags LanguageStyle
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        ]
        [ imageView
            [ width $ V 32
            , height $ V 32
            , cornerRadius 32.0
            , padding $ Padding 8 8 8 8
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_right_arrow_blue"
            , background Color.blue100
            ]
        ]
    ]
  where
  marginTop = if index == 0 then 0 else 16

settingSideBarView :: forall w. (Action -> Effect Unit) -> ST.TicketingScreenState -> PrestoDOM (Effect Unit) w
settingSideBarView push state =
  linearLayout
    [ height  MATCH_PARENT
    , width MATCH_PARENT
    , visibility if state.data.sideBarStatus == SettingSideBar.OPEN then VISIBLE else GONE
    ]
    [ SettingSideBar.view (push <<< SettingSideBarActionController) (settingSiderBarState state) ]
