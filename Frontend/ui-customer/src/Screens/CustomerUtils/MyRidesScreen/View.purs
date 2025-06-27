{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyRidesScreen.View where

import Common.Types.App
import Screens.CustomerUtils.MyRidesScreen.ComponentConfig

import Animation as Anim
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Data.Array ((..))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (getArray)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), (<$>), (<>), (&&), (<<<), (==), (||), const, show, discard, bind, not, pure, unit, when, void, map)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, clickable, color, gravity, height, id, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, scrollBarY, swipeRefreshLayout, text, textView, visibility, weight, width, textSize, fontStyle, lineHeight, enableRefresh , setEnable)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.List as PrestoList
import PrestoDOM.Types.Core (toPropValue)
import Screens.MyRidesScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.API (RideBookingListRes(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (GlobalState, defaultGlobalState)
import Helpers.Utils as HU
import Mobility.Prelude (boolToVisibility)
import Debug(spy)
import Data.Maybe(Maybe(..))

screen :: ST.MyRidesScreenState -> PrestoList.ListItem -> Screen Action ST.MyRidesScreenState ScreenOutput
screen initialState listItemm =
  {
    initialState : initialState {
      shimmerLoader = ST.AnimatedIn
    }
  , view : view listItemm
  , name : "MyRidesScreen"
  , globalEvents : [
       globalOnScroll "MyRidesScreen",
        ( \push -> do
                    void $ launchAff $ EHC.flowRunner defaultGlobalState $ getPastRides RideBookingListAPIResponseAction push initialState
                    pure $ pure unit
        ),globalNotificationListener
  ]
  , eval:
      \action state -> do
        let _ = spy "MyRidesScreen action " action
        let _ = spy "MyRidesScreen state  " state
        eval action state
  }
  where
  globalNotificationListener push = do
    void $ HU.storeCallBackCustomer push NotificationListener "MyRidesScreen" Just Nothing
    pure $ pure unit


view :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> ST.MyRidesScreenState -> PrestoDOM (Effect Unit) w
view listItemm push state =
  Anim.screenAnimation $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
  , onBackPressed push $ const BackPressed
  , afterRender push (const AfterRender)
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , weight 1.0
      ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
        , if (not state.data.config.nyBrandingVisibility) then
            separatorView
          else
            linearLayout[][]
        , if state.data.config.nyBrandingVisibility && HU.showTitle FunctionCall then do
            textView $
              [ text (getString SELECT_A_RIDE)
              , color Color.black700
              , padding (Padding 16 0 16 0)
              ] <> FontStyle.body1 LanguageStyle
          else do
            (linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.blue600
            , visibility if (not (DA.null state.itemsRides)) then VISIBLE else GONE
            , orientation HORIZONTAL
            , padding (Padding 16 12 16 16)
            ][  textView $
                [ text (getString SELECT_A_RIDE)
                , color Color.black700
                ] <> FontStyle.body1 LanguageStyle
            ])
        , ridesView listItemm push state
        ]
    , loadButtonView state push]

loadButtonView :: forall w. ST.MyRidesScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
loadButtonView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , onClick push (const Loader)
  , clickable state.data.loadMoreText
  , gravity CENTER
  , alignParentBottom "true,-1"
  , padding (Padding 0 0 0 5)
  , visibility if (state.props.loaderButtonVisibility && (not state.props.loadMoreDisabled)) then VISIBLE else GONE
  ][ linearLayout[
    background Color.grey900
  , width MATCH_PARENT
  , height $ V 1
  ][]
    ,linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 0 5 0 5
    ]
    [ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ if state.data.loadMoreText then getString LOAD_MORE else getString NO_MORE_RIDES
      , padding (Padding 10 5 10 5)
      , color Color.blue900
      ] <> FontStyle.body1 LanguageStyle
    ]]

ridesView :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> ST.MyRidesScreenState -> PrestoDOM (Effect Unit) w
ridesView listItemm push state =
  swipeRefreshLayout
  ([height MATCH_PARENT
  , width MATCH_PARENT
  , onRefresh push (const Refresh)
  , enableRefresh state.props.refreshLoader
  ]<> if state.props.scrollEnable then [setEnable $ false] else [] )
  [ Keyed.relativeLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ]([ Tuple "Rides"
        $ PrestoList.list
        [ height MATCH_PARENT
        , scrollBarY false
        , width MATCH_PARENT
        , onScroll "rides" "MyRidesScreen" push (Scroll )
        , onScrollStateChange push (ScrollStateChanged)
        , visibility $ case DA.null state.itemsRides of
                    false -> VISIBLE
                    true -> GONE
        , PrestoList.listItem listItemm
        , background Color.white900
        , PrestoList.listDataV2 $ (DA.filter (\item -> DA.any (_ == item.status) $ map (toPropValue) ["COMPLETED", "CANCELLED", "REALLOCATED", "CONFIRMED","UPCOMING","TRIP_ASSIGNED"]) state.prestoListArrayItems)
        ]
      , Tuple "NoRides"
        $ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , background Color.white900
          , visibility $ boolToVisibility $ DA.null state.itemsRides
          ][  ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig state)]
      , Tuple "APIFailure"
        $ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , background Color.white900
          , gravity CENTER
          , visibility if state.props.apiFailure then VISIBLE else GONE
          ][  ErrorModal.view (push <<< APIFailureActionController) (apiErrorModalConfig state)]
      , Tuple "LOADER"
          $ PrestoAnim.animationSet
          [ PrestoAnim.Animation
            [ PrestoAnim.duration 1000
            , PrestoAnim.toAlpha $
                case state.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedIn] -> 1.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedOut] -> 0.0
                  _ -> 0.0
            , PrestoAnim.fromAlpha $
                case state.shimmerLoader of
                  loader | loader `DA.elem` [ST.AnimatingIn, ST.AnimatedOut] -> 0.0
                  loader | loader `DA.elem` [ST.AnimatingOut, ST.AnimatedIn] -> 1.0
                  _ -> 0.0
            , PrestoAnim.tag "Shimmer"
            ] true
          ] $ PrestoList.list
            [ height MATCH_PARENT
            , scrollBarY false
            , background Color.whiteSmoke
            , width MATCH_PARENT
            , onAnimationEnd push OnFadeComplete
            , PrestoList.listItem listItemm
            , PrestoList.listDataV2 $ shimmerData <$> (getArray 5)
            , visibility $ case state.shimmerLoader of
                    ST.AnimatedOut -> GONE
                    _ -> VISIBLE
            ]
      ])
  ]

separatorView :: forall w. PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.greySmoke
  ][]

shimmerData :: Int -> ST.ItemState
shimmerData i = {
  date : toPropValue "31/05/2022",
  time : toPropValue "7:35pm",
  source : toPropValue "Nagarjuna Apartments,15/2, 19th Main, 27th Cross Rd, Sector 2, HSR Layout, Bengaluru, Karnataka 560102",
  destination : toPropValue "Nagarjuna Apartments,15/2, 19th Main, 27th Cross Rd, Sector 2, HSR Layout, Bengaluru, Karnataka 560102",
  totalAmount : toPropValue "€ 0.0",
  cardVisibility : toPropValue "gone",
  shimmerVisibility : toPropValue "visible",
  driverImage : toPropValue "",
  isCancelled : toPropValue "visible",
  isSuccessfull :toPropValue "gone",
  rating : toPropValue "",
  driverName : toPropValue "",
  rideStartTime : toPropValue "",
  rideEndTime : toPropValue "",
  vehicleNumber : toPropValue "",
  rideId : toPropValue "",
  status : toPropValue "",
  rideEndTimeUTC : toPropValue "",
  alpha : toPropValue "",
  zoneVisibility : toPropValue "gone",
  variantImage : toPropValue "",
  showVariantImage : toPropValue "",
  showRepeatRide : toPropValue "visible",
  isScheduled : toPropValue "gone",
  showDestination : toPropValue "visible",
  itemRideType : toPropValue "ONE_WAY",
  rideTypeVisibility : toPropValue "gone",
  rideTypeBackground : toPropValue "#454545",
  cornerRadius : toPropValue "24.0"
}

getPastRides :: forall action.( RideBookingListRes -> String -> action) -> (action -> Effect Unit) -> ST.MyRidesScreenState ->  Flow GlobalState Unit
getPastRides action push state = do
  let fromBanner = if state.props.fromBanner then "true" else "false"
  (rideBookingListResponse) <- Remote.rideBookingList "8" (show state.data.offsetValue) fromBanner
  case rideBookingListResponse of
      Right (RideBookingListRes  listResp) -> do
          doAff do liftEffect $ push $ action (RideBookingListRes listResp) "success"
          pure unit
      Left (err) -> do
        doAff do liftEffect $ push $ action (RideBookingListRes dummyListResp ) if err.code == 500 then "listCompleted" else "failure"
        pure unit

dummyListResp :: forall a.  { list :: Array a}
dummyListResp = {list : []}
