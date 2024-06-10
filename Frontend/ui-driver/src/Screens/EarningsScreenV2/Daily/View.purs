module Screens.EarningsScreen.Daily.View where

import Common.Types.App
import Data.Array
import Data.FoldableWithIndex
import Data.Maybe
import Debug
import Effect
import Helpers.Utils
import Mobility.Prelude
import Prelude
import PrestoDOM hiding (tabLayout)
import Screens.EarningsScreen.Common.Types
import Screens.EarningsScreen.Common.View
import Screens.EarningsScreen.Daily.Controller
import Screens.EarningsScreen.Daily.ComponentConfig
import Screens.EarningsScreen.ScreenData
import Components.Calendar.View as Calendar
import Services.API
import Components.BottomNavBar.View as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Animation as Anim
import Effect.Aff (launchAff, killFiber, launchAff_, error)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.API as API
import JBridge (getHeightFromPercent, getWidthFromPercent, getCurrentDate, getDateFromDate)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Styles.Colors as Color
import Screens as ScreenNames
import Types.App (defaultGlobalState)
import Data.Either
import Presto.Core.Types.Language.Flow (fork, await, doAff, Flow)
import Data.String as DS
import Data.Function.Uncurried (runFn2)
import Timers (debounceCallBackWithId)
import Effect.Uncurried (runEffectFn3)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..), runBackT)
import Screens.EarningsScreen.Common.Utils

screen :: State -> Screen Action State ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "EarningsScreenV2Daily"
  , globalEvents: [ (\push -> fetchRideSummary push initialState) ]
  , eval: (\action state -> eval (spy "EarningsScreenDaily action" action) (spy "EarningsScreenDaily state" state))
  }

fetchRideSummary :: (Action -> Effect Unit) -> State -> Effect (Effect Unit)
fetchRideSummary push state = do
  fiber <-
    launchAff $ flowRunner defaultGlobalState
      $ do
          let
            isToday = state.data.currentDate == "Today"

            currentDate = DS.replaceAll (DS.Pattern "/") (DS.Replacement "-") (if isToday then getCurrentDate else state.data.currentDate)

            mbExisting = if isToday then Nothing else getRideData currentDate
          case mbExisting of
            Nothing -> do
              listControl <-
                fork
                  $ do
                      eiResp <- Remote.getRideHistoryReq "100" "0" "false" "null" currentDate
                      case eiResp of
                        Right (GetRidesHistoryResp rideHistoryResponse) -> do
                          EHC.liftFlow $ push $ UpdateRideHistory rideHistoryResponse.list state.data.currentDate
                          pure $ Just $ (GetRidesHistoryResp rideHistoryResponse)
                        Left err -> do
                          void $ pure $ spy "Error GetRidesHistoryResp" err
                          pure $ Nothing
              handleSummary currentDate (Just listControl)
            Just existing -> do
              case existing.noListFetched of
                true -> do
                  void $ fork
                    $ do
                        eiResp <- Remote.getRideHistoryReq "100" "0" "false" "null" currentDate
                        case eiResp of
                          Right (GetRidesHistoryResp rideHistoryResponse) -> do
                            EHC.liftFlow $ push $ UpdateRideHistory rideHistoryResponse.list state.data.currentDate
                            EHC.liftFlow $ updateRideDatas [] rideHistoryResponse.list currentDate false
                            pure $ Just $ (GetRidesHistoryResp rideHistoryResponse)
                          Left err -> do
                            void $ pure $ spy "Error GetRidesHistoryResp" err
                            pure $ Nothing
                false -> EHC.liftFlow $ push $ UpdateRideHistory (if existing.noRidesTaken then [] else existing.list) state.data.currentDate
              if existing.noSummaryFound then handleSummary currentDate Nothing else EHC.liftFlow $ push $ UpdateRideData $ if (existing.noRidesTaken) then [] else [ fromRideDataToRidesSummary existing ]
              pure unit
  pure $ (launchAff_ $ killFiber (error "Failed to Cancel") fiber)
  where
  handleSummary currentDate mbListControl = do
    resp <- API.callApi (GetRidesSummaryListReq [ currentDate ])
    case resp of
      Right (GetRidesSummaryListResp rideSummaryResp) -> do
        EHC.liftFlow $ push $ UpdateRideData rideSummaryResp.list
        case mbListControl of
          Nothing -> pure unit
          Just listControl -> do
            mbResp <- await listControl
            case mbResp of
              Just (GetRidesHistoryResp rideHistoryResponse) -> EHC.liftFlow $ updateRideDatas rideSummaryResp.list rideHistoryResponse.list currentDate false
              Nothing -> EHC.liftFlow $ updateRideDatas rideSummaryResp.list [] currentDate true
      Left err -> void $ pure $ spy "Error GetRidesSummaryListResp" err

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , afterRender push $ const AfterRender
    , onBackPressed push (const BackClick)
    ]
    $ [ defaultLayout push state
      -- , rideDistanceInfoPopUp push state
      ]
    <> if state.data.calendarState.calendarPopup then
        [ PrestoAnim.animationSet
            [ Anim.fadeIn (state.data.calendarState.calendarPopup)
            ]
            $ Calendar.view (push <<< CalendarAC) (calendarConfig state)
        ]
      else []

defaultLayout :: forall w. (Action -> Effect Unit) -> State -> Layout w
defaultLayout push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , padding $ PaddingBottom EHC.safeMarginBottom
    ]
    [ headerLayout push GoToHelpAndSupport state
    , relativeLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , weight 1.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            ]
            [ scrollView
                [ width MATCH_PARENT
                , height MATCH_PARENT
                , scrollBarY false
                ]
                [ linearLayout
                    [ orientation VERTICAL
                    , width MATCH_PARENT
                    , height MATCH_PARENT
                    ]
                    [ case state.data.selectedDate of
                        Nothing -> earnignsTopView push state true (dummyRideSummaryType state.data.currentDate)
                        Just val -> earnignsTopView push state false val
                    , earningsInfoView push state
                    , case state.data.selectedDateRides of
                        Nothing -> rideHistoryView push state true []
                        Just item -> rideHistoryView push state false item
                    ]
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ Margin 16 16 16 0
            ]
            [ tabLayout push ChangeTab TAB_DAILY state.props.isFromWeekly state.props.isResetAnim
            ]
        ]
    , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN state.data.config.bottomNavConfig)
    ]

earnignsTopView :: forall w. (Action -> Effect Unit) -> State -> Boolean -> RidesSummaryType -> Layout w
earnignsTopView push state showShimmer item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 24
    , gradient $ Linear 180.0 [ "#E0D1FF", "#E0D1FF", "#F9F6FF" ]
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , visibility INVISIBLE
        ]
        [ linearLayout
          ( [ width MATCH_PARENT
            , height WRAP_CONTENT
            , cornerRadius 14.0
            , gravity CENTER
            ]
          )
          [ textView
              $ [ text $ "Hello Dummy string"
                , margin $ MarginVertical 6 6
                ]
              <> FontStyle.tags TypoGraphy
          ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , margin $ MarginTop 32
        ]
        [ linearLayout
            [ weight 1.0
            , height MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 16
            , onClick
                ( \_ -> do
                    let
                      date = getDate DECREMENT
                    push $ UpdateDate date
                    runEffectFn3 debounceCallBackWithId "DailyStats" (getDateCallback date) 1000
                )
                $ const NoAction
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            ]
            [ dateSelectionView push state showShimmer item
            , if showShimmer then
                sfl (getHeightFromPercent 4) (getWidthFromPercent 30) "#66FFFFFF"
              else
                textView
                  $ [ text item.earningsWithCurrency
                    , color Color.black800
                    ]
                  <> FontStyle.priceFont TypoGraphy
            -- , textView -- TODO enable after getting tips
            --     $ [ text "$72 in Tips Included"
            --       , margin $ MarginTop 14
            --       , color Color.black800
            --       ]
            --     <> FontStyle.subHeading2 TypoGraphy
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER
            , padding $ PaddingVertical 16 16
            , onClick
                ( \action ->
                    if state.props.forwardBtnAlpha == 1.0 then do
                      let
                        date = getDate INCREMENT
                      push $ UpdateDate date
                      runEffectFn3 debounceCallBackWithId "DailyStats" (getDateCallback date) 1000
                    else
                      pure unit
                )
                $ const NoAction
            , alpha state.props.forwardBtnAlpha
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                ]
            ]
        ]
    , ridesStatsView push state showShimmer item
    ]
  where
  getDate incrementType = case incrementType of
    INCREMENT -> runFn2 getDateFromDate (getDateFromCurrentDate state.data.currentDate) 1
    DECREMENT -> runFn2 getDateFromDate (getDateFromCurrentDate state.data.currentDate) (-1)

  getDateCallback updatedDate = push $ ChangeDate updatedDate

dateSelectionView :: forall w. (Action -> Effect Unit) -> State -> Boolean -> RidesSummaryType -> Layout w
dateSelectionView push state showShimmer item =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ Padding 15 4 15 4
    , margin $ MarginBottom 14
    , gravity CENTER_VERTICAL
    , background "#66FFFFFF"
    , cornerRadius 12.0
    , onClick push $ const ShowCalendarPopup
    ]
    [ imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar"
        , margin $ MarginRight 6
        ]
    , textView
        $ [ text state.data.currentDate
          , color Color.black800
          , margin $ Margin 0 0 6 (if __IS_ANDROID then 2 else 0)
          ]
        <> FontStyle.tags TypoGraphy
    , imageView
        [ height $ V 12
        , width $ V 12
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_down"
        , margin $ MarginLeft 4
        ]
    ]

ridesStatsView :: forall w. (Action -> Effect Unit) -> State -> Boolean -> RidesSummaryType -> Layout w
ridesStatsView push state showShimmer item =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , margin $ MarginTop 32
    , cornerRadius 12.0
    , padding $ Padding 10 10 10 10
    ]
    ( foldlWithIndex
        ( \idx acc item ->
            let
              isNotlastIndex = idx /= 2
            in
              acc
                <> ( [ linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , padding $ PaddingHorizontal 18 (if isNotlastIndex then 18 else 0)
                        , gravity CENTER
                        ]
                        $ [ linearLayout
                              [ height WRAP_CONTENT
                              , gravity CENTER
                              , width WRAP_CONTENT
                              , orientation VERTICAL
                              ]
                              [ textView
                                  $ [ text item.title
                                    , color Color.black800
                                    , margin $ MarginTop 4
                                    ]
                                  <> FontStyle.tags TypoGraphy
                              , if showShimmer then
                                  sfl (getHeightFromPercent 1) (getWidthFromPercent 10) Color.grey900
                                else
                                  textView
                                    $ [ text item.value
                                      , color Color.black800
                                      ]
                                    <> FontStyle.tags TypoGraphy
                              ]
                          ]
                    ]
                  )
                <> ( if isNotlastIndex then
                      [ linearLayout
                          [ height WRAP_CONTENT
                          , weight 1.0
                          , gravity CENTER
                          ]
                          [ linearLayout
                              [ width $ V 2
                              , height $ V 30
                              , background Color.grey900
                              ]
                              []
                          ]
                      ]
                    else
                      []
                  )
        )
        []
        [ { title: "Rides", value: item.noOfRides }, { title: "Total Ride Dist", value: item.rideDistanceWithUnit }, { title: "Total Ride Time", value: "--" } ]
    )

earningsInfoView :: forall w. (Action -> Effect Unit) -> State -> Layout w
earningsInfoView push state =
  PrestoAnim.animationSet
    [ Anim.expandWithDuration 100 (state.props.showInfoView)
    , Anim.collapseWithDuration 100 (not state.props.showInfoView)
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background "#F9F6FF"
        , margin $ Margin 16 16 16 0
        , cornerRadius 12.0
        , padding $ Padding 16 12 16 12
        , gravity CENTER_VERTICAL
        , enableAnimateOnGone true
        , visibility $ boolToVisibility state.props.showInfoView
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , weight 1.0
            , orientation VERTICAL
            ]
            [ textView
                $ [ text "Your daily earnings will be transferred to your account in 4 days from the date of earning"
                  , color Color.black700
                  ]
                <> FontStyle.body3 TypoGraphy
            , textView
                $ [ text "Learn more"
                  , color Color.purple700
                  ]
                <> FontStyle.body3 TypoGraphy
            ]
        , imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
            , margin $ MarginLeft 10
            , onClick push $ const ToggleInfoView
            ]
        ]

rideHistoryView :: forall w. (Action -> Effect Unit) -> State -> Boolean -> Array RideComponent -> Layout w
rideHistoryView push state showShimmer datas =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    ]
    [ textView
        $ [ text "Ride History"
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , relativeLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ PrestoAnim.animationSet
            [ Anim.fadeOut $ not showShimmer
            ]
            $ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , margin $ MarginTop 12
                , visibility $ boolToVisibility showShimmer
                , enableAnimateOnGone true
                ]
                ( mapWithIndex
                    (\idx item -> shimmerRideComponent (idx + 1) item)
                    listDatas
                )
        , if showShimmer then
            linearLayout [] []
          else if datas == [] then
            linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , stroke $ "1," <> Color.grey900
              , gravity CENTER
              , cornerRadius 10.0
              , margin $ MarginTop 12
              , padding $ Padding 16 32 16 32
              ]
              [ textView
                  $ [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ getString NO_RIDES
                    ]
                  <> FontStyle.subHeading1 TypoGraphy
              , textView
                  $ [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text $ getString YOU_DID_NOT_TAKE_ANY_RIDES_ON_PREFIX <> " " <> (if (state.data.currentDate == getCurrentDate) then "Today" else DS.replaceAll (DS.Pattern "-") (DS.Replacement "/") state.data.currentDate) <> " " <> getString YOU_DID_NOT_TAKE_ANY_RIDES_ON_SUFFIX
                    ]
                  <> FontStyle.paragraphText TypoGraphy
              ]
          else
            linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , margin $ MarginTop 12
              ]
              ( mapWithIndex
                  (\idx item -> rideComponent push (OpenSelectedTripDetails idx) (idx + 1) item)
                  datas
              )
        ]
    ]

rideDistanceInfoPopUp :: forall w. (Action -> Effect Unit) -> State -> Layout w
rideDistanceInfoPopUp push state =
  PrestoAnim.animationSet
    [ Anim.fadeInWithDuration 100 (state.props.rideDistanceInfoPopUp)
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height $ V $ EHC.screenHeight unit
        , background Color.blackLessTrans
        , onClick push $ const RemovePopup
        , gravity BOTTOM
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.rideDistanceInfoPopUp
        ]
        [ linearLayout
            [ height $ V $ getHeightFromPercent 30
            , width MATCH_PARENT
            , background Color.white900
            , orientation VERTICAL
            , padding $ Padding 16 20 16 (EHC.safeMarginBottomWithDefault 16)
            , cornerRadii $ Corners 24.0 true true false false
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ textView
                    $ [ text "Ride Distance"
                      , color Color.black900
                      , margin $ MarginBottom 4
                      , weight 1.0
                      ]
                    <> FontStyle.subHeading1 TypoGraphy
                , linearLayout
                    [ height $ V 24
                    , width $ V 24
                    , gravity CENTER
                    , onClick push $ const RemovePopup
                    ]
                    [ imageView
                        [ height $ V 16
                        , width $ V 16
                        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
                        ]
                    ]
                ]
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , margin $ MarginVertical 16 10
                , background Color.grey900
                ]
                []
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                ( map
                    ( \item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , margin $ MarginVertical 10 10
                          ]
                          [ textView
                              $ [ text item.text
                                , color Color.black800
                                , weight 1.0
                                ]
                              <> FontStyle.body6 TypoGraphy
                          , textView
                              $ [ text item.value
                                , color Color.black800
                                ]
                              <> FontStyle.body6 TypoGraphy
                          ]
                    )
                    [ { text: "The Pickup distance"
                      , value: "10 mi"
                      }
                    , { text: "Ride Dist."
                      , value: "60 mi"
                      }
                    ]
                )
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , margin $ MarginTop 10
                , background Color.grey900
                ]
                []
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ MarginTop 20
                , gravity CENTER
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
                    , height $ V 16
                    , width $ V 16
                    , margin $ MarginRight 5
                    ]
                , textView
                    $ [ text "The pickup distance is the distance you travelled to the pickup point."
                      , color Color.black800
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
        ]
