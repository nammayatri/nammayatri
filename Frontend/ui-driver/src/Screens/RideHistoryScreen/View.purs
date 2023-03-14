module Screens.RideHistoryScreen.View where

import Prelude ( Unit, ($), (<$>), const, (==), (<<<), bind, pure, unit, discard, show, not, (&&),($), (<$>), (<>),(<<<), (==), (/), (>))
import Screens.Types as ST
import PrestoDOM (Gravity(..), Length(..), Orientation(..), Margin(..), Visibility(..), PrestoDOM, Padding(..), Screen, color, gravity, height, width,id, alignParentBottom, linearLayout, onScroll, margin, orientation, onScrollStateChange, padding, text, textSize, textView, background, scrollBarY, swipeRefreshLayout, onAnimationEnd, visibility, weight, onRefresh, onClick, onBackPressed, afterRender)
import Effect (Effect)
import Screens.RideHistoryScreen.Controller (Action(..), ScreenOutput, eval, prestoListFilter)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM.List as PrestoList
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple as DT
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Types.Core (toPropValue)
import Data.Array as DA
import Data.Array ((..))
import Components.ErrorModal as ErrorModal
import Styles.Colors as Color
import Components.BottomNavBar as BottomNavBar
import Engineering.Helpers.Commons (safeMarginBottom, screenWidth)
import Services.Backend as Remote
import PrestoDOM.Events (globalOnScroll)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Animation as Anim
import Services.APITypes (GetRidesHistoryResp(..), Status(..))
import Common.Types.App
import Components.BottomNavBar.Controller (navData)
import Screens.RideHistoryScreen.ComponentConfig


screen :: ST.RideHistoryScreenState -> PrestoList.ListItem -> Screen Action ST.RideHistoryScreenState ScreenOutput
screen initialState rideListItem = 
  {
    initialState : initialState {
      shimmerLoader = ST.AnimatedIn
    }
  , view : view rideListItem
  , name : "RideHistoryScreen"
  , globalEvents : [
    globalOnScroll "RideHistoryScreen",
        ( \push -> do
            launchAff_ $ flowRunner $ runExceptT $ runBackT $ do
              (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "8" (show initialState.offsetValue) "false"
              lift $ lift $ doAff do liftEffect $ push $ RideHistoryAPIResponseAction rideHistoryResponse.list
            pure $ pure unit
        )
  ]
  , eval
  }

view :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> ST.RideHistoryScreenState -> PrestoDOM (Effect Unit) w
view rideListItem push state = 
   linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , afterRender push (const AfterRender)
    ][ Anim.screenAnimationFadeInOut $
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        ][ headerView push state
          , separatorView 
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
      , visibility if (state.loaderButtonVisibility && (not state.loadMoreDisabled)) then VISIBLE else GONE--(state.data.totalItemCount == (state.data.firstVisibleItem + state.data.visibleItemCount) && state.data.totalItemCount /= 0 && state.data.totalItemCount /= state.data.visibleItemCount) then VISIBLE else GONE
      ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation VERTICAL
        , margin $ Margin 0 5 0 5
        ]
        [ textView
          ([ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString LOAD_MORE)
          , padding (Padding 10 5 10 5)
          , color Color.blueTextColor
          ] <> FontStyle.subHeading1 TypoGraphy)
        ]
    ]
    , BottomNavBar.view (push <<< BottomNavBarAction) (navData 1)
    ]


headerView :: forall w . (Action -> Effect Unit) -> ST.RideHistoryScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , margin (MarginTop 30)
    ][ textView
        [ text (getString RIDES)
        , gravity CENTER_VERTICAL
        , textSize FontSize.a_20
        , color Color.black900
        , margin (MarginBottom 10)
        ]
      , linearLayout
         [ background Color.bg_color
         , orientation HORIZONTAL
         , width MATCH_PARENT
         , height WRAP_CONTENT 
         ][ linearLayout
              [ orientation VERTICAL
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , weight 0.5
              , onClick push (const $ SelectTab "COMPLETED")
              ][
                linearLayout
                  [ width $ (V (screenWidth unit / 2) )
                  , height WRAP_CONTENT
                  , gravity CENTER
                  ][ textView  
                      [ text (getString COMPLETED_)
                      , textSize FontSize.a_18
                      , color if state.currentTab == "COMPLETED" then Color.black900 else Color.black500
                      , margin (MarginVertical 15 15)
                      ]
                  ]
              , linearLayout
                  [ height (V 2)
                  , width MATCH_PARENT
                  , background Color.black900
                  , visibility if state.currentTab == "COMPLETED" then VISIBLE else GONE
                  ][]
              ]
          , linearLayout
              [ orientation VERTICAL
              , width WRAP_CONTENT
              , height WRAP_CONTENT
              , weight 0.5
              , onClick push (const $ SelectTab "CANCELLED")
              ][
                linearLayout
                  [ width $ V (screenWidth unit / 2)
                  , height WRAP_CONTENT
                  , gravity CENTER
                  ][ textView  
                      [ text (getString CANCELLED_) 
                      , textSize FontSize.a_18
                      , color if state.currentTab == "CANCELLED" then Color.black900 else Color.black500
                      , margin (MarginVertical 15 15)
                      ]
                  ]
              , linearLayout
                  [ height (V 2)
                  , width MATCH_PARENT
                  , background Color.black900
                  , visibility if state.currentTab == "CANCELLED" then VISIBLE else GONE
                  ][]
              ]
         ]
    ]


ridesView :: forall w . PrestoList.ListItem -> (Action -> Effect Unit) -> ST.RideHistoryScreenState -> PrestoDOM (Effect Unit) w
ridesView rideListItem push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ][ swipeRefreshLayout
      [height MATCH_PARENT
      , width MATCH_PARENT
      , onRefresh push (const Refresh)
      , id "2000030"
      ]
      [ Keyed.relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ]([ DT.Tuple "Rides"
            $ PrestoList.list
            [ height MATCH_PARENT
            , scrollBarY false 
            , width MATCH_PARENT
            , onScroll "rides" "RideHistoryScreen" push (Scroll)
            , onScrollStateChange push (ScrollStateChanged)
            , visibility $ case state.shimmerLoader of
                        ST.AnimatedOut -> VISIBLE
                        _ -> GONE
            , PrestoList.listItem rideListItem 
            , background Color.bg_grey
            , PrestoList.listDataV2 (prestoListFilter state.currentTab state.prestoListArrayItems)
            ]
          , DT.Tuple "LOADER"
              $ PrestoAnim.animationSet
              [ PrestoAnim.Animation
                [ PrestoAnim.duration 1000
                , PrestoAnim.toAlpha $
                    case state.shimmerLoader of
                        ST.AnimatingIn -> 1.0
                        ST.AnimatedIn -> 1.0
                        ST.AnimatingOut -> 0.0
                        ST.AnimatedOut -> 0.0
                , PrestoAnim.fromAlpha $
                    case state.shimmerLoader of
                        ST.AnimatingIn -> 0.0
                        ST.AnimatedIn -> 1.0
                        ST.AnimatingOut -> 1.0
                        ST.AnimatedOut -> 0.0
                , PrestoAnim.tag "Shimmer"
                ] true
              ] $ PrestoList.list
                [ height MATCH_PARENT
                , scrollBarY false
                , background Color.bg_grey
                , width MATCH_PARENT
                , onAnimationEnd push OnFadeComplete
                , PrestoList.listItem rideListItem
                , PrestoList.listDataV2 $ shimmerData <$> (1..5)
                , visibility $ case state.shimmerLoader of
                        ST.AnimatedOut -> GONE
                        _ -> VISIBLE
                ]
          , DT.Tuple "NoRides"
              $ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , padding (PaddingBottom safeMarginBottom)
                , background Color.white900
                , visibility $ case state.shimmerLoader of
                          ST.AnimatedOut ->  if DA.length (prestoListFilter state.currentTab state.prestoListArrayItems) > 0 then GONE else VISIBLE
                          _ -> GONE
                ][  ErrorModal.view (push <<< ErrorModalActionController) (errorModalConfig)]
          ])
        ]

  ]

  

separatorView :: forall w. PrestoDOM (Effect Unit) w
separatorView = 
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background Color.separatorViewColor
  ][]

shimmerData :: Int -> ST.ItemState 
shimmerData i = {
  date : toPropValue "31/05/2022",
  time : toPropValue "7:35pm",
  total_amount : toPropValue "₹ 0.0",
  card_visibility : toPropValue "gone",
  shimmer_visibility : toPropValue "visible",
  ride_distance_visibility : toPropValue "visible",
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
  amountColor: toPropValue ""
}
