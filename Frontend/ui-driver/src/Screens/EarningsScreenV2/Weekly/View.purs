module Screens.EarningsScreen.Weekly.View where

import Animation as Anim
import Animation.Config (Direction(..), animConfig)
import Common.Types.App
import Components.BottomNavBar.View as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array
import Data.FoldableWithIndex
import Data.Int
import Data.Maybe
import Data.String as DS
import Debug
import Effect
import Effect.Aff (launchAff, killFiber, launchAff_, error)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (intPriceToBeDisplayed, intDistanceTobeDisplayed)
import Font.Style as FontStyle
import Helpers.API as API
import Helpers.Utils
import JBridge (getHeightFromPercent, getWidthFromPercent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Locale.Utils
import Mobility.Prelude
import PrestoDOM hiding (tabLayout)
import PrestoDOM.Events (globalOnScroll)
import PrestoDOM.Types.DomAttributes (__IS_ANDROID)
import PrestoDOM.Animation as PrestoAnim
import Prelude
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types as ST
import Screens.EarningsScreen.Weekly.Controller
import Screens.EarningsScreen.ScreenData
import Screens.EarningsScreen.Common.Types
import Screens.EarningsScreen.Common.View
import Screens.EarningsScreen.Common.Utils
import Services.API
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState, GlobalState(..))
import Screens as ScreenNames
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))

screen :: State -> Screen Action State ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "EarningsScreenV2"
  , globalEvents:
      [ globalOnScroll "EarningsScreenV2"
      , (\push -> fetchAndUpdateRideSummary push initialState)
      ]
  , eval: (\action state -> eval (spy "EarningsScreenWeekly action" action) (spy "EarningsScreenWeekly state" state))
  }

fetchAndUpdateRideSummary :: (Action -> Effect Unit) -> State -> Effect (Effect Unit)
fetchAndUpdateRideSummary push state = do
  fiber <-
    launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
      $ do
          let
            datesList = getDatesListV2 (getcurrentdate "") state.props.fromDate state.props.toDate state.props.isCurrentWeek

            listOfDates = filter (\date -> isNothing $ getRideData date) datesList
          if null listOfDates then
            liftFlowBT $ updateRideSummary [] (getcurrentdate "") datesList push state
          else do
            (GetRidesSummaryListResp rideSummaryResp) <- Remote.getRideSummaryListReqBT listOfDates
            liftFlowBT $ updateRideSummary rideSummaryResp.list (getcurrentdate "") datesList push state
          pure unit
  pure $ (launchAff_ $ killFiber (error "Failed to Cancel") fiber)


updateRideSummary :: Array RidesSummary -> String -> Array String -> (Action -> Effect Unit) -> State -> Effect Unit
updateRideSummary ridesSummaryList todaysDate datesList push state = do
  let
    fromDate = fromMaybe "" (datesList !! 0)
  if state.props.toDate /= "" && state.props.toDate /= "" && not state.props.isCurrentWeek then do
    let
      missingDatesFromList = getMissingDatesFromList ridesSummaryList datesList

      _ = map (\date -> updateRideDatas [] [] date false) missingDatesFromList

      _ = map (\(RidesSummary rideSummary) -> updateRideDatas [ (RidesSummary rideSummary) ] [] rideSummary.rideDate true) ridesSummaryList

      getRideDataForWeek = map (\date -> fromMaybe (dummyRideData date) (getRideData date)) datesList

      earningsForWeek = mapRideDataWithEarnings getRideDataForWeek

      currentWeekData = getWeeklyEarningsPercentage earningsForWeek

      currWeekMaxEarning = foldl getMax 0 currentWeekData
    push $ UpdateRideSummary currentWeekData currWeekMaxEarning fromDate false (getTotalCurrentWeekData currentWeekData)
  else do
    let
      currWeekDatesBeforeToday = map (\obj -> EHC.convertUTCtoISC obj.utcDate "YYYY-MM-DD") $ EHC.getPastDays todaysDate (EHC.getDayOfWeek $ EHC.getDayName todaysDate)

      missingDatesFromList = getMissingDatesFromList ridesSummaryList currWeekDatesBeforeToday

      ridesSummaryListBeforTodaysDate = filter (\(RidesSummary list) -> list.rideDate /= todaysDate) ridesSummaryList

      _ = map (\date -> updateRideDatas [] [] date false) missingDatesFromList

      _ = map (\(RidesSummary rideSummary) -> updateRideDatas [ (RidesSummary rideSummary) ] [] rideSummary.rideDate true) ridesSummaryListBeforTodaysDate

      dayOfWeek = EHC.getDayOfWeek (EHC.getDayName todaysDate)

      noOfDaysToNearestSunday = 6 - dayOfWeek

      datesUptoMearestSunday = if noOfDaysToNearestSunday > 0 then map (\x -> EHC.getFutureDate todaysDate x) (1 .. noOfDaysToNearestSunday) else []

      getRideDataForWeek = map (\date -> fromMaybe (dummyRideData date) (getRideData date)) (datesList <> datesUptoMearestSunday)

      earningsForWeek = mapRideDataWithEarnings getRideDataForWeek

      currentWeekData = getWeeklyEarningsPercentage earningsForWeek

      currWeekMaxEarning = foldl getMax 0 currentWeekData
    push $ UpdateRideSummary currentWeekData currWeekMaxEarning fromDate true (getTotalCurrentWeekData currentWeekData)

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    ]
    $ [ defaultLayout push state
      ]

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
        [ weeklyEarningContentView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ Margin 16 16 16 0
            ]
            [ tabLayout push ChangeTab TAB_WEEKLY true false
            ]
        ]
    , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_EARNINGS_SCREEN state.data.config.bottomNavConfig)
    ]

weeklyEarningContentView :: forall w. (Action -> Effect Unit) -> State -> Layout w
weeklyEarningContentView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , scrollBarY false
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            ]
            [ earnignsTopView push state
            , rideHistoryView push state
            , earningSummary push state
            , seeHistoryButtons push state
            ]
        ]
    ]

earnignsTopView :: forall w. (Action -> Effect Unit) -> State -> Layout w
earnignsTopView push state =
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
            , onClick push $ const (GetWeeklyEarnings true)
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
            [ textView
                $ [ text $ getText state
                  , color Color.black800
                  , minHeight 24
                  ]
                <> FontStyle.subHeading1 TypoGraphy
            , textView
                $ [ text $ intPriceToBeDisplayed state.props.totalWeeklyEarningsdata.totalEarningsWithCurrency true
                  , margin $ MarginTop 14
                  , color Color.black800
                  ]
                <> FontStyle.priceFont TypoGraphy
            -- , textView -- TODO Enable after Tips
            --     $ [ text "$72 in Tips Included"
            --       , margin $ MarginTop 16
            --       , color Color.black800
            --       ]
            --     <> FontStyle.subHeading2 TypoGraphy
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER
            , clickable $ not state.props.isCurrentWeek
            , onClick push $ const (GetWeeklyEarnings false)
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                ]
            ]
        ]
    , ridesStatsView push state
    ]
  where
  getText state = if state.props.fromDate /= "" then (convertUTCtoISC state.props.fromDate "MMM DD") <> " - " <> (convertUTCtoISC state.props.toDate "MMM DD") else "__ - __"

dateSelectionView :: forall w. (Action -> Effect Unit) -> State -> Layout w
dateSelectionView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ Padding 15 4 15 4
    , gravity CENTER_VERTICAL
    , background "#66FFFFFF"
    , cornerRadius 12.0
    ]
    [ imageView
        [ height $ V 16
        , width $ V 16
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_calendar"
        , margin $ MarginRight 6
        ]
    , textView
        $ [ text "Today"
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

ridesStatsView :: forall w. (Action -> Effect Unit) -> State -> Layout w
ridesStatsView push state =
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
              islastIndex = idx /= 2
            in
              acc
                <> ( [ linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , padding $ PaddingHorizontal 18 (if islastIndex then 18 else 0)
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
                                    ]
                                  <> FontStyle.tags TypoGraphy
                              , textView
                                  $ [ text item.value
                                    , margin $ MarginTop 4
                                    , color Color.black800
                                    ]
                                  <> FontStyle.tags TypoGraphy
                              ]
                          ]
                    ]
                  )
                <> ( if islastIndex then
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
        [ { title: "Rides", value: show state.props.totalWeeklyEarningsdata.totalRides }, { title: "Total Ride Dist", value: intDistanceTobeDisplayed state.props.totalWeeklyEarningsdata.totalDistanceTravelledWithUnit true true }, { title: "Total Ride Time", value: "--" } ]
    )

rideHistoryView :: forall w. (Action -> Effect Unit) -> State -> Layout w
rideHistoryView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , stroke $ "1," <> Color.grey900
    , padding $ Padding 16 12 16 12
    , cornerRadius 8.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , alpha 0.4
            , onClick push $ const (GetWeeklyEarnings true)
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        , textView
            $ [ height $ WRAP_CONTENT
              , weight 1.0
              , text $ getText state
              , gravity CENTER
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , rotation 180.0
            , alpha 0.4
            , clickable $ not state.props.isCurrentWeek
            , onClick push $ const (GetWeeklyEarnings false)
            ]
            [ imageView
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
                ]
            ]
        ]
    , barGraphView push state
    ]
  where
  getText state = if state.props.fromDate /= "" then (convertUTCtoISC state.props.fromDate "MMM DD") <> " - " <> (convertUTCtoISC state.props.toDate "MMM DD") else "__ - __"

barGraphView :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
barGraphView push state =
  let
    currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
    relativeLayout
      [ height $ V 150
      , width MATCH_PARENT
      ]
      [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ dottedLineView push 4 currWeekMaxEarning
          , dottedLineView push 37 ((currWeekMaxEarning * 2) / 3)
          , dottedLineView push 70 (currWeekMaxEarning / 3)
          , linearLayout
              [ width MATCH_PARENT
              , height $ WRAP_CONTENT
              , orientation VERTICAL
              , margin $ MarginTop 10
              ]
              [ linearLayout
                  [ height $ V 2
                  , width MATCH_PARENT
                  , background Color.grey900
                  ]
                  []
              , textView
                  $ [ height $ WRAP_CONTENT
                    , width $ MATCH_PARENT
                    , text $ "12"
                    , visibility INVISIBLE
                    ]
                  <> FontStyle.body3 TypoGraphy
              , textView
                  $ [ height $ WRAP_CONTENT
                    , width $ MATCH_PARENT
                    , text $ "MON"
                    , visibility INVISIBLE
                    ]
                  <> FontStyle.body3 TypoGraphy
              ]
          ]
      , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , weight 1.0
          , background Color.transparent
          , orientation HORIZONTAL
          , gravity BOTTOM
          ]
          $ [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , weight 5.0
                , background Color.transparent
                , orientation HORIZONTAL
                , gravity BOTTOM
                ]
                (mapWithIndex (\index item -> (barView push index item state)) state.props.currWeekData)
            , linearLayout
                [ weight 1.0
                , width WRAP_CONTENT
                , gravity CENTER
                ]
                [ textView
                    $ [ height WRAP_CONTENT
                      , width WRAP_CONTENT
                      , margin $ MarginLeft 4
                      , visibility INVISIBLE
                      , text $ "₹1200"
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                ]
            ]
      ]

dottedLineView :: forall w. (Action -> Effect Unit) -> Int -> Int -> PrestoDOM (Effect Unit) w
dottedLineView push margintop earnings =
  linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    , gravity CENTER
    ]
    [ imageView
        [ height $ V 2
        , width MATCH_PARENT
        , weight 1.0
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_dotted_line"
        ]
    , textView
        $ [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , margin $ MarginLeft 4
          , text $ "$" <> (show earnings)
          ]
        <> FontStyle.paragraphText TypoGraphy
    ]

barView :: forall w. (Action -> Effect Unit) -> Int -> WeeklyEarning -> State -> PrestoDOM (Effect Unit) w
barView push index item state =
  let
    selectedIndex = state.props.selectedBarIndex

    setMargin = case index of
      0 -> MarginHorizontal 3 6
      6 -> MarginHorizontal 3 (EHC.screenWidth unit / 8)
      _ -> MarginHorizontal 6 6
  in
    linearLayout
      [ weight 1.0
      , height MATCH_PARENT
      , orientation VERTICAL
      , alignParentBottom "true,-1"
      , gravity BOTTOM
      ]
      [ Keyed.linearLayout
          [ height $ V if item.percentLength > 0.0 then (ceil item.percentLength) else 1
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ if item.percentLength > 0.0 then
              Tuple item.rideDate
                $ ( if EHC.os == "IOS" then
                      PrestoAnim.animationSet [ Anim.expandWithDuration 1000 (state.props.startBarAnim && item.percentLength > 0.0) ]
                    else
                      PrestoAnim.animationSet [ Anim.translateInYAnim $ animConfig { duration = 1000 + (ceil item.percentLength), fromY = (ceil item.percentLength), ifAnim = (state.props.startBarAnim && item.percentLength > 0.0) } ]
                  )
                $ linearLayout
                    ( [ height $ V if item.percentLength > 0.0 then (ceil item.percentLength) else 1
                      , width $ V $ getWidthFromPercent 7
                      , background if item.percentLength > 20.0 then Color.purple700 else Color.red900
                      , cornerRadius 4.0
                      ]
                        <> if item.percentLength > 0.0 then [ pivotY 1.0 ] else []
                    )
                    []
            else
              Tuple (item.rideDate <> "_defaultLayout")
                $ ( if EHC.os == "IOS" then
                      PrestoAnim.animationSet [ Anim.expandWithDuration 1000 state.props.startBarAnim ]
                    else
                      PrestoAnim.animationSet [ Anim.translateInYAnim $ animConfig { duration = 1000 + (ceil item.percentLength), fromY = (ceil item.percentLength), ifAnim = state.props.startBarAnim } ]
                  )
                $ linearLayout
                    ( [ height $ V 1
                      , width $ V $ getWidthFromPercent 7
                      , background Color.red900
                      , cornerRadius 4.0
                      ]
                        <> if item.percentLength > 0.0 then [ pivotY 1.0 ] else []
                    )
                    []
          ]
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ convertUTCtoISC item.rideDate "DD"
            , color Color.black700
            , gravity CENTER
            ]
          <> FontStyle.body3 TypoGraphy
      , textView
          $ [ height $ WRAP_CONTENT
            , width $ MATCH_PARENT
            , text $ (fromMaybe "" (EHC.getWeekDays !! index))
            , gravity CENTER
            , singleLine true
            , color Color.black700
            , margin $ if (getLanguageLocale languageKey == "KN_IN") then MarginBottom 0 else MarginBottom 2
            ]
          <> FontStyle.body3 TypoGraphy
      ]

earningSummary :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
earningSummary push state =
  let
    showAdj = state.data.adjustmentRotation == 180.0
  in
    linearLayout
      [ height $ WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginHorizontal 16 16
      , padding $ Padding 16 16 16 16
      , stroke $ "1," <> Color.grey900
      , cornerRadius 8.0
      ]
      [ textView
          $ [ height $ WRAP_CONTENT
            , width WRAP_CONTENT
            , text "Earnings Summary"
            , color Color.black800
            ]
          <> FontStyle.subHeading1 TypoGraphy
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 0) [] true
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 17) [] true
      , dropDown push "Adjustments" state.data.adjustmentRotation state.data.prevAdjustmentRotation ShowAdjustments
      , tableViewCell state (FontStyle.body6 TypoGraphy) Color.black800 (MarginTop 13) [ Anim.expandWithDuration 150 showAdj, Anim.collapseWithDuration 150 (not showAdj) ] showAdj
      , dropDown push "Tips" 270.0 270.0 ShowTips
      , linearLayout
          [ height $ V 1
          , background Color.grey900
          , width MATCH_PARENT
          , margin $ MarginTop 17
          ]
          []
      , tableViewCell state (FontStyle.body22 TypoGraphy) Color.green900 (MarginTop 17) [] true
      ]

dropDown :: forall w. (Action -> Effect Unit) -> String -> Number -> Number -> Action -> PrestoDOM (Effect Unit) w
dropDown push content rotate prevRotate action =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginTop 17
    , onClick push $ const action
    ]
    [ textView
        $ [ height $ WRAP_CONTENT
          , weight 1.0
          , text $ content
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , PrestoAnim.animationSet
        ( if rotate == 270.0 then
            []
          else
            [ Anim.rotateToDegWithDuration 150 (ceil prevRotate) (ceil rotate) ]
        )
        $ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , alpha 0.4
            ]
            [ imageView
                [ height $ V 10
                , width $ V 10
                , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_down"
                ]
            ]
    ]

tableViewCell state fontStyle col marg anim vis =
  PrestoAnim.animationSet anim
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin marg
        , visibility $ boolToVisibility vis
        , enableAnimateOnGone true
        ]
        [ textView
            $ [ text "Passenger Payment"
              , color col
              , weight 1.0
              , height WRAP_CONTENT
              ]
            <> fontStyle
        , textView
            $ [ text "Passenger Payment"
              , color col
              ]
            <> fontStyle
        ]

seeHistoryButtons :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
seeHistoryButtons push state =
  let
    array = [ "See Ride History", "See Payout History", "Get Help" ]
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 16 16 16 16
      , margin $ MarginVertical 12 12
      , orientation VERTICAL
      ]
      ( mapWithIndex
          ( \index item ->
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation HORIZONTAL
                    , onClick push $ const $ OptionClickedFromList index
                    ]
                    [ textView
                        ( [ height WRAP_CONTENT
                          , weight 1.0
                          , text $ item
                          ]
                            <> FontStyle.subHeading1 TypoGraphy
                        )
                    , imageView
                        [ height $ V 24
                        , width $ V 24
                        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
                        ]
                    ]
                , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , padding $ Padding 16 16 16 16
                    , margin $ MarginVertical 12 12
                    , background Color.grey900
                    , visibility $ boolToVisibility $ (index /= length array - 1)
                    ]
                    []
                ]
          )
          array
      )
