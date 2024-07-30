module Screens.ScheduledRideAcceptedScreen.View where

import Animation
import Debug
import Debug
import Prelude
import PrestoDOM.List
import Screens.ScheduledRideAcceptedScreen.Controller
import Screens.ScheduledRideAcceptedScreen.ScreenData

import Animation as Anim
import Common.Types.App (LazyCheck(..), CategoryListType)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Int (ceil, fromString, round, toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os, safeMarginBottom, getFutureDate)
import Engineering.Helpers.Commons (screenWidth)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import JBridge as JB
import Language.Strings (getString)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (BottomSheetState(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), adjustViewWithKeyboard, afterRender, alignParentBottom, alpha, background, bottomSheetLayout, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, halfExpandedRatio, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, lineHeight, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, peakHeight, relativeLayout, singleLine, stroke, text, textSize, textView, onScrollStateChange, visibility, weight, width, topShift, onAnimationEnd, horizontalScrollView, scrollBarX, setEnable)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, frameLayout, orientation, padding, text, textSize, textView, weight, width, onClick, layoutGravity, alpha, scrollView, cornerRadius, onBackPressed, stroke, lineHeight, visibility, afterRender, scrollBarY, imageWithFallback, rippleColor, clickable, relativeLayout, alignParentBottom, id, onAnimationEnd, swipeRefreshLayout, onRefresh, onScroll, nestedScrollView, enableRefresh)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Properties (alpha, cornerRadii, lineHeight, minWidth)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Services.API (ScheduledBookingListResponse(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (FlowBT, GlobalState(..))
import Types.App (defaultGlobalState)

screen :: ScheduleRideAcceptedScreenState -> Screen Action ScheduleRideAcceptedScreenState ScreenOutput
screen initialState =  
  { initialState
  , view : view
  , name: "ScheduleRideAcceptScreen"
  , globalEvents : [] 
  , eval : (\action state -> do
    let _ = spy "ScheduleRideAcceptScreen state -----" state
    let _ = spy "ScheduleRideAcceptScreen action --------" action
    eval action state)
  }

view :: forall w. (Action -> Effect Unit) -> ScheduleRideAcceptedScreenState -> PrestoDOM (Effect Unit) w 
view state push  = 
    linearLayout [
        height MATCH_PARENT
    ,   width MATCH_PARENT
    ,   background Color.grey700
     

    ][
     linearLayout[
        height MATCH_PARENT
        ,width MATCH_PARENT
        ,orientation VERTICAL
        ]
        [linearLayout[
            weight 1.0
            ,width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            ][
            imageView [
             width $ V 180
             ,height $ V 150
             ,margin $ Margin 0 0 0 30
             ,imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ride_accepted_symbol"
             ]
             , textView $ [
                height WRAP_CONTENT
               ,width WRAP_CONTENT
               ,text $ getString SCHEDULED_RIDE_ACCEPTED
               , color Color.black900
             ]<>FontStyle.h1 TypoGraphy
             , textView $ [
                height WRAP_CONTENT
               ,width WRAP_CONTENT
               , margin $ MarginTop 8
               ,text $ getString YOU_CAN_ACCESS_SCHEDULED_RIDES
             ]<>FontStyle.h3 TypoGraphy
             , textView $ [
                height WRAP_CONTENT
               ,width WRAP_CONTENT
               ,text $ getString FROM_YOUR_HOMESCREEN
             ]<>FontStyle.h3 TypoGraphy
            ]
            ,buttonView state push
        ]
    
    ]

buttonView :: forall w. (Action -> Effect Unit) -> ScheduleRideAcceptedScreenState -> PrestoDOM (Effect Unit) w 
buttonView push state  = 
   linearLayout [
        height $ V 50 
        , width MATCH_PARENT
        , background Color.black900
        , margin $ Margin 10 0 10 10 
        , cornerRadius  8.0
        , gravity CENTER
        ,  onClick push (const  OnClick)
        , rippleColor Color.rippleShade
     
    ][
        textView $ [
            width WRAP_CONTENT
            , height $ V 40
            , text $ getString CONTINUE
            , color Color.yellow900
            , gravity CENTER
        ]<>FontStyle.h3 TypoGraphy
    ]

