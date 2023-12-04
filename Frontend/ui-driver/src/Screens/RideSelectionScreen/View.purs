{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.View where

import Common.Types.App
import Animation (screenAnimation, screenAnimationFadeInOut)
import Components.BottomNavBar.Controller (navData)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, (..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, os, safeMarginBottom, screenWidth)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, show, unit, ($), (&&), (/), (<$>), (<<<), (<>), (==), (>))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, Visibility(..), afterRender, alignParentBottom, background, color, gravity, height, id, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, scrollBarY, swipeRefreshLayout, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Elements.Elements (imageView)
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (alpha, fontStyle, imageUrl, imageWithFallback, layoutGravity, lineHeight)
import PrestoDOM.Types.Core (toPropValue)
import Screens.RideSelectionScreen.ComponentConfig (cancelButtonConfig, errorModalConfig)
import Screens.RideSelectionScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (AnimationState(..), ItemState, RideSelectionScreenState)
import Services.API (GetRidesHistoryResp(..), Status(..))
import Components.BottomNavBar as BottomNavBar
import Styles.Colors as Color
import Components.ErrorModal as ErrorModal
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.List as PrestoList
import Components.PrimaryButton (view) as PrimaryButton
import Services.Backend as Remote
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Data.Maybe (Maybe(..))

screen :: RideSelectionScreenState -> PrestoList.ListItem -> ScopedScreen Action RideSelectionScreenState ScreenOutput
screen initialState rideListItem =
  { initialState : initialState {
      shimmerLoader = AnimatedIn
    }
  , view : view rideListItem
  , name : "RideSelectionScreen"
  , globalEvents : [
      globalOnScroll "RideSelectionScreen"
    , (\ push -> do
                 _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                   (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "8" (show initialState.offsetValue) "false" "COMPLETED" "null"
                   lift $ lift $ doAff do liftEffect $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
                 pure $ pure unit
      )
    ]
  , eval
  , parent: Nothing
  }

view :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> RideSelectionScreenState -> PrestoDOM (Effect Unit) w
view rideListItem push state =
  screenAnimation
  $ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ][ linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , orientation VERTICAL
       , weight 1.0
       ][ headerLayout state push
        , selectARideHeader state push
        , ridesView rideListItem push state
        ]
     , linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , orientation VERTICAL
       , background Color.white900
       , onClick push (const Loader)
       , gravity CENTER
       , alignParentBottom "true,-1"
       , padding (PaddingBottom 5)
       , visibility if (state.loaderButtonVisibility && (not state.loadMoreDisabled)) then VISIBLE else GONE --(state.data.totalItemCount == (state.data.firstVisibleItem + state.data.visibleItemCount) && state.data.totalItemCount /= 0 && state.data.totalItemCount /= state.data.visibleItemCount) then VISIBLE else GONE
       ][ linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.greyLight
          ][]
        , textView
         ([ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString LOAD_MORE)
          , margin $ Margin 0 5 0 5
          , padding (Padding 10 5 10 5)
          , color Color.blueTextColor
          ]
          <> FontStyle.subHeading1 TypoGraphy
         )
        ]
       , linearLayout
         [ width MATCH_PARENT
         , height $ V 1
         , background Color.greyLight
         ][]
       , PrimaryButton.view (push <<< DontKnowRide) (cancelButtonConfig state)
     ]


headerLayout :: RideSelectionScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     , orientation HORIZONTAL
     , gravity CENTER_VERTICAL
     , layoutGravity "center_vertical"
     , padding $ Padding 5 16 5 16
     ][ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
        , onClick push $ const BackPressed
        , padding $ Padding 2 2 2 2
        , margin $ MarginLeft 5
        ]
      , textView
        ([ width WRAP_CONTENT
         , height WRAP_CONTENT
         , text $ getCategoryName state.selectedCategory.categoryAction
         , textSize FontSize.a_18
         , margin $ MarginLeft 20
         , weight 1.0
         , color Color.black900
         ]
         <> FontStyle.h3 TypoGraphy
        )
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background Color.greyLight
      ][ ]
   ]

selectARideHeader :: RideSelectionScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
selectARideHeader state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 15 10 10 10)
  , background Color.lightGreyBlue
  ][ textView
     [ width WRAP_CONTENT
     , height MATCH_PARENT
     , text (getString RIDE_REPORT_ISSUE)
     , gravity CENTER_VERTICAL
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , textSize FontSize.a_18
     , color Color.black800
     , lineHeight "25"
     ]
   , linearLayout
     [ width MATCH_PARENT
     , height MATCH_PARENT
     ][ textView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , textSize FontSize.a_17
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity RIGHT
        , color Color.blueTextColor
        ]
      ]
   ]

ridesView :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> RideSelectionScreenState -> PrestoDOM (Effect Unit) w
ridesView rideListItem push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ][ swipeRefreshLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , onRefresh push (const Refresh)
     , id "2000030"
     ][ Keyed.relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]
        ([ Tuple "Rides" $
           PrestoList.list
           [ height MATCH_PARENT
           , scrollBarY false
           , width MATCH_PARENT
           , onScroll "rides" "RideSelectionScreen" push (Scroll)
           , onScrollStateChange push (ScrollStateChanged)
           , visibility $ case state.shimmerLoader of
                       AnimatedOut -> VISIBLE
                       _ -> GONE
           , PrestoList.listItem rideListItem
           , PrestoList.listDataV2 (state.prestoListArrayItems)
           ]
         , Tuple "LOADER" $
           PrestoAnim.animationSet
           [ PrestoAnim.Animation
             [ PrestoAnim.duration 1000
             , PrestoAnim.toAlpha $
                 case state.shimmerLoader of
                   AnimatingIn -> 1.0
                   AnimatedIn -> 1.0
                   AnimatingOut -> 0.0
                   AnimatedOut -> 0.0
             , PrestoAnim.fromAlpha $
                 case state.shimmerLoader of
                   AnimatingIn -> 0.0
                   AnimatedIn -> 1.0
                   AnimatingOut -> 1.0
                   AnimatedOut -> 0.0
             , PrestoAnim.tag "Shimmer"
             ] true
           ] $
             PrestoList.list
             [ height MATCH_PARENT
             , scrollBarY false
             , background Color.white900
             , width MATCH_PARENT
             , onAnimationEnd push OnFadeComplete
             , PrestoList.listItem rideListItem
             , PrestoList.listDataV2 $ shimmerData <$> (1..5)
             , visibility $ case state.shimmerLoader of
                              AnimatedOut -> GONE
                              _ -> VISIBLE
             ]
         , Tuple "NoRides" $
           linearLayout
           [ height MATCH_PARENT
           , width MATCH_PARENT
           , padding (PaddingBottom safeMarginBottom)
           , background Color.white900
           , visibility $ case state.shimmerLoader of
                            AnimatedOut ->  if length state.prestoListArrayItems > 0 then GONE else VISIBLE
                            _ -> GONE
           ][ ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig)]
         ]
        )
      ]
   ]


shimmerData :: Int -> ItemState
shimmerData i =
  { date : toPropValue "31/05/2022",
  time : toPropValue "7:35pm",
  total_amount : toPropValue "₹ 0.0",
  card_visibility : toPropValue "gone",
  shimmer_visibility : toPropValue "visible",
  rideDistance : toPropValue "10km Ride with Bharat",
  status :  toPropValue "",
  vehicleModel : toPropValue "Auto",
  shortRideId : toPropValue ""  ,
  vehicleNumber :  toPropValue ""  ,
  driverName : toPropValue ""  ,
  driverSelectedFare : toPropValue ""  ,
  vehicleColor : toPropValue ""  ,
  id : toPropValue "",
  updatedAt : toPropValue "",
  source : toPropValue "Nagarjuna Apartments,15/2, 19th Main, 27th Cross Rd, Sector 2, HSR Layout, Bengaluru, Karnataka 560102",
  destination : toPropValue "Nagarjuna Apartments,15/2, 19th Main, 27th Cross Rd, Sector 2, HSR Layout, Bengaluru, Karnataka 560102",
  amountColor: toPropValue "",
  riderName : toPropValue "",
  spLocTagVisibility : toPropValue "",
  gotoTagVisibility : toPropValue "",
  specialZoneText : toPropValue "",
  specialZoneImage : toPropValue "",
  specialZoneLayoutBackground : toPropValue "",
  purpleTagVisibility : toPropValue "",
  tipTagVisibility : toPropValue ""
  }

getCategoryName :: String -> String
getCategoryName categoryName = case categoryName of
  "LOST_AND_FOUND" -> (getString REPORT_LOST_ITEM)
  "RIDE_RELATED" -> (getString RIDE_RELATED_ISSUE)
  "APP_RELATED" -> (getString APP_RELATED_ISSUE)
  "FARE" -> (getString FARE_RELATED_ISSUE)
  _ -> ""
