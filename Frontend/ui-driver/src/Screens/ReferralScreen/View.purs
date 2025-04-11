{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.View where


import Animation (screenAnimationFadeInOut)
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Data.Array (mapWithIndex, (!!), length, index, drop, (..), last, any)
import Data.Int (toNumber, ceil)
import Data.String (take, length, null) as DS
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os, getNewIDWithTag, flowRunner, screenWidth, getCurrentUTC, getPastMonths)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (openUrlInApp, toast)
import Language.Strings 
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>), map, discard, show, (>), void, (/=), (/), (*), (+), not, (||), negate, (<=), (&&), (-), (<))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Gradient(..), background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility, stroke, alpha, relativeLayout, scrollView, alignParentRight, alignParentBottom, imageWithFallback, frameLayout, horizontalScrollView, scrollBarX, scrollBarY, id, gradient, rotation, rotationY, shimmerFrameLayout, onRefresh,  swipeRefreshLayout, layoutGravity, textFromHtml, Orientation, Length)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.Types as ST
import Services.API (LeaderBoardReq(..), GetPerformanceReq(..), GetPerformanceRes(..), GenerateReferralCodeReq(..), GenerateReferralCodeRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Styles.Types as Font
import Types.App (defaultGlobalState)
import Components.PopUpModal as PopUpModal
import Data.Maybe (Maybe(..), fromMaybe, fromJust, isJust)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Helpers.Utils (setRefreshing, getPastWeeks, convertUTCtoISC, getPastDays, getcurrentdate, fetchImage, FetchImageFrom(..))
import Screens.ReferralScreen.ComponentConfig
import Screens as ScreenNames
import Data.Either (Either(..))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))
import Debug (spy)
import Timers (startTimer)
import Mobility.Prelude(boolToVisibility)
import Common.Types.App as CT

screen :: ST.ReferralScreenState -> Screen Action ST.ReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ReferralScreen"
  , globalEvents : [
              (\push -> do
                void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                  case initialState.props.stage of 
                    ST.LeaderBoard -> do
                        leaderBoardRes <- lift $ lift $ Remote.leaderBoard $ 
                          case initialState.props.leaderBoardType of
                            ST.Daily  ->
                              let selectedDay =  if DS.null initialState.props.selectedDay.utcDate then
                                                    fromMaybe initialState.props.selectedDay $ last $ getPastDays 1
                                                  else initialState.props.selectedDay
                              in DailyRequest $ convertUTCtoISC selectedDay.utcDate "YYYY-MM-DD"
                            ST.Weekly ->
                              let selectedWeek =  if any DS.null [initialState.props.selectedWeek.utcStartDate, initialState.props.selectedWeek.utcEndDate] then
                                                    fromMaybe initialState.props.selectedWeek $ last $ getPastWeeks 1
                                                  else initialState.props.selectedWeek
                              in WeeklyRequest (convertUTCtoISC selectedWeek.utcStartDate "YYYY-MM-DD") $ convertUTCtoISC selectedWeek.utcEndDate "YYYY-MM-DD"
                            ST.Monthly ->
                              let initialSelectedMonth = initialState.props.selectedMonth
                                  selectedMonth = if DS.null initialSelectedMonth.utcDate then
                                                    fromMaybe initialSelectedMonth $ last $ getPastMonths 1
                                                  else initialSelectedMonth
                              in MonthlyRequest selectedMonth.month
                        lift $ lift $ doAff do liftEffect $ push $ case leaderBoardRes of
                                                                    Right res -> UpdateLeaderBoard res
                                                                    Left err  -> UpdateLeaderBoardFailed
                        pure unit
                    ST.QRScreen -> do 
                      let refCode = getValueToLocalStore REFERRAL_CODE
                      (GetPerformanceRes getPerformanceres) <- Remote.getPerformanceBT (GetPerformanceReq {} )
                      lift $ lift $ doAff do liftEffect $ push $ UpdateDriverPerformance (GetPerformanceRes getPerformanceres)
                      if (any (_ == refCode) ["__failed", "", "(null)"]) then do
                          response <- lift $ lift $ Remote.generateReferralCode (GenerateReferralCodeReq {} )
                          case response of
                            Right (GenerateReferralCodeRes generateReferralCodeRes) -> lift $ lift $ doAff do liftEffect $ push $ (UpdateReferralCode (GenerateReferralCodeRes generateReferralCodeRes))
                            Left error -> pure unit
                      else pure unit 
                    _ -> pure unit
                pure (pure unit)
              )
  ]
  , eval : (\action state -> do
      let _ = spy "Referral state -----" state
      let _ = spy "Referral--------action" action
      eval action state)
  }


view :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut $
    relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ](
    [linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , background Color.white900
    , gravity CENTER
    , afterRender push (const AfterRender)
    ][  linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        , height WRAP_CONTENT
        ](
        [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ][ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
           , linearLayout
             [ width MATCH_PARENT
             , height MATCH_PARENT
             , margin (MarginRight 16)
             , gravity RIGHT
             ][ linearLayout
                [ width WRAP_CONTENT
                , height MATCH_PARENT
                , gravity CENTER
                ][ linearLayout
                   [ width WRAP_CONTENT
                   , height WRAP_CONTENT
                   , gravity CENTER
                   , background Color.grey700
                   , padding (Padding 11 6 11 6)
                   , orientation HORIZONTAL
                   , cornerRadius 8.0
                   , visibility GONE
                   ][ imageView
                      [ width (V 16)
                      , height (V 16)
                      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_reward"
                      , margin (MarginRight 5)
                      ]
                    , textView
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text $ "(" <> (getString COMING_SOON) <> ")"
                      , gravity CENTER
                      , textSize FontSize.a_14
                      , color Color.black600
                      ]
                   ]
                ]
             ]
          ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
        ] <> if state.props.stage == ST.SuccessScreen then [commonView push "ny_ic_green_tick" (getString  YOUR_REFERRAL_CODE_IS_LINKED) (getString YOU_CAN_NOW_EARN_REWARDS) state] else []
          <> if state.props.stage == ST.ComingSoonScreen then [commonView push "ny_ic_comming_soon_poster" (getString COMING_SOON) (getString COMING_SOON_DESCRIPTION) state] else []
          <> if state.props.stage == ST.ReferralFlow then  [referralEnrolmentFlow push state, continueButtonView push state] else []
          <> if state.props.stage == ST.QRScreen then [qrScreenView push state] else []
          <> if state.props.stage == ST.LeaderBoard then [leaderBoard push state] else [])
        , bottomNavBarView push state
        ]
        , passwordPopUpView push state
        , customerSupportPopUpView state push
    ] <> if state.props.passwordPopUpVisible then [passwordPopUpView push state] else [])

shimmerView :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , visibility if state.props.showShimmer then VISIBLE else GONE
  ][  linearLayout
      [ width MATCH_PARENT
      , height (V 235)
      , margin (Margin 16 15 16 0)
      , background Color.greyDark
      , cornerRadius 16.0
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin (MarginTop 258)
      ](mapWithIndex (\index item ->
                      linearLayout
                      [ width MATCH_PARENT
                      , height (V 60)
                      , margin (Margin 16 16 16 0)
                      , cornerRadius 12.0
                      , background Color.greyDark
                      ][]
                  ) (1 .. 7)
       )
  ]

leaderBoard :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoard push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (Margin 16 12 16 0)
      , padding (Padding 2 2 2 2)
      , background Color.grey800
      , cornerRadius 30.0
      , gravity CENTER
      ]([ leaderBoardTab (getStringEnToHi DAILY) ST.Daily push state
       , leaderBoardTab (getStringEnToHi WEEKLY) ST.Weekly push state
       ] <> if state.data.config.showMonthlyLeaderBoard then [leaderBoardTab (getStringEnToHi MONTHLY) ST.Monthly push state] else [])
    , dateAndTime push state
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      ]([ leaderBoardRanksCover push state
        , dateSelector push state
          ]<> if (state.props.currentDriverData.rank > 10 || state.props.currentDriverData.rank < 1) then [currentDriverRank state] else []
      )
   ]

noDataView :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
noDataView state =
   scrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin (Margin 16 15 16 15)
        , gradient (Linear 0.0 [Color.lightGradientBlue, Color.darkGradientBlue])
        , gravity CENTER
        , padding (Padding 24 30 24 8)
        , cornerRadius 16.0
        , orientation VERTICAL
        ][  imageView
            [ width $ V $ (screenWidth unit) - 48
            , height $ V $ ceil ((toNumber ((screenWidth unit) - 48)) * 1.25)
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_leaderboard_no_data"
            ]
          , textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin (Margin 24 0 24 4)
            , text (getStringEnToHi GETTING_THE_LEADERBOARD_READY)
            , color Color.black800
            , fontStyle $ FontStyle.bold LanguageStyle
            , textSize FontSize.a_18
            , gravity CENTER_HORIZONTAL
            ]
          , textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin (Margin 24 0 24 30)
            , text (getStringEnToHi PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS)
            , color Color.black700
            , textSize FontSize.a_14
            , gravity CENTER_HORIZONTAL
            ]
         ]
      ]
     ]
  

currentDriverRank :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
currentDriverRank state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , cornerRadii $ Corners 18.0 true true false false
  , visibility if( checkDate state || state.props.currentDriverData.rank > 0 ) && not state.props.showShimmer then VISIBLE else GONE
  ][ rankCard state.props.currentDriverData true state
   ]

dateAndTime :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
dateAndTime push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginHorizontal 16 16
  , padding $ Padding 8 15 8 15
  , gravity CENTER_VERTICAL
  ]
  [ linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , weight 1.0
    , onClick push $ const $ DateSelectorAction
    ]
    [ textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text marginDateText
      , color Color.black800
      ] <> FontStyle.body32 LanguageStyle
    , imageView
      [ width $ V 24
      , height $ V 24
      , margin $ MarginLeft 12
      , imageWithFallback $ fetchImage FF_ASSET $ if state.props.showDateSelector then "ny_ic_chevron_up_blue" else "ny_ic_calendar_blue"
      ]
    ]
  , textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getStringEnToHi LAST_UPDATED <> state.props.lastUpdatedAt
    , color Color.black700
    , gravity RIGHT
    , weight 1.0
    ] <> FontStyle.body9 LanguageStyle
  ]
  where
    marginDateText :: String
    marginDateText = 
      let date = state.props.selectedDay
          week = state.props.selectedWeek
          month = state.props.selectedMonth
      in case state.props.leaderBoardType of
          ST.Daily -> show date.date <> " " <> date.month <> ", " <> show date.year
          ST.Weekly -> show week.startDate <> " " <> week.startMonth <> " - " <> show week.endDate <> " " <> week.endMonth
          ST.Monthly -> convertUTCtoISC month.utcDate "MMMM" <> ", " <> convertUTCtoISC month.utcDate "YYYY"

leaderBoardRanksCover :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoardRanksCover push state = 
  swipeRefreshLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , onRefresh push $ const RefreshScreen
  , id (getNewIDWithTag "ReferralRefreshView")
  ][ if state.props.noData then
      noDataView state
     else
      leaderBoardRanks state
  ]
  
leaderBoardRanks :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoardRanks state =
  scrollView
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  ][  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingBottom if (state.props.currentDriverData.rank > 10 ||( checkDate state && state.props.currentDriverData.rank < 1)) then 68 else 8)
    ][ shimmerView state
     , congratsBar state
     , topRankers state
     , otherRankers state
     ]
  ]

congratsBar :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
congratsBar state =
  let rank = state.props.currentDriverData.rank
  in
  linearLayout
  ([ width MATCH_PARENT
   , height (V 44)
   , margin (Margin 16 15 16 0)
   , cornerRadius 8.0
   , visibility if rank <= 10 && rank > 0 && not state.props.showShimmer then VISIBLE else GONE
   ] <> if rank == 1 then [gradient (Linear 270.0 ["#FEE8B8","#D3A119","#C59311", "#E1BC5B"])] else [background Color.blue800]
  )[  frameLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ]([  linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity CENTER
          ][  textView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text  if rank == 1 then
                        (getStringEnToHi CONGRATULATIONS_YOU_ARE_RANK) <> (show rank) <> " 🏆"
                      else
                        (getStringEnToHi CONGRATULATIONS_YOU_ARE_RANK) <> (show rank) <> " 🎉"
              , color Color.white900
              , textSize FontSize.a_18
              ]
           ]
       ] <> if rank > 0 && rank <= 3 then [shineAnimation rank] else [])
   ]

shineAnimation :: forall w . Int -> PrestoDOM (Effect Unit) w
shineAnimation rank =
  PrestoAnim.animationSet
  [ PrestoAnim.Animation
    [ PrestoAnim.duration 2500
    , PrestoAnim.fromX $ (- 200)
    , PrestoAnim.toX $ (screenWidth unit) + 200
    , PrestoAnim.repeatCount PrestoAnim.Infinite
    ] true
  ] $ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ linearLayout
        [ width (V 10)
        , height (V 200)
        , margin (MarginRight 20)
        ][]
      , linearLayout
        [ width (V 10)
        , height (V 100)
        , background Color.transparentWhite
        , rotation 20.0
        , margin (MarginRight 10)
        ][]
      , linearLayout
        [ width (V 25)
        , height (V 100)
        , background Color.transparentWhite
        , rotation 20.0
        ][]
      ]

otherRankers :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
otherRankers state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility if not state.props.showShimmer then VISIBLE else GONE
  ] (mapWithIndex (\index item ->
                      rankCard item false state
                  ) (drop 3 state.props.rankersData)
    )

rankCard :: forall w . ST.RankCardData -> Boolean -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
rankCard item aboveThreshold state =
  let currentDriverData = state.props.currentDriverData
      driverImage = case item.gender of
                      "MALE" -> "ny_ic_general_profile"
                      "FEMALE" -> "ny_ic_general_profile_female"
                      _ -> "ny_ic_generic_mascot"
  in
    linearLayout
    ([ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ (if aboveThreshold then "0," else "1,") <> Color.grey900
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding if aboveThreshold then (Padding 24 10 32 10) else (Padding 8 10 16 10)
    , background if aboveThreshold || (item == currentDriverData && currentDriverData.rank > 0) then Color.blue800 else Color.white900
    ] <> (if not aboveThreshold then [margin (Margin 16 8 16 8)] else [])
      <> (if aboveThreshold then [cornerRadii (Corners 12.0 true true false false)] else [cornerRadius 12.0])
    )[ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ linearLayout
          [ width (V 55)
          , height WRAP_CONTENT
          , gravity CENTER_HORIZONTAL
          , margin (MarginHorizontal 8 6)
          , visibility $ if checkDriverWithZeroRides item aboveThreshold state then GONE else VISIBLE
          ][ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ if item.rank > 0 then show item.rank else "-"
            , textSize FontSize.a_14
            , color if aboveThreshold || (item == currentDriverData && currentDriverData.rank > 0) then Color.white900 else Color.black800
            , fontStyle  $ FontStyle.semiBold LanguageStyle
            ]
          ]
        , imageView
          [ width (V 40)
          , height (V 40)
          , cornerRadius 30.0
          , margin (MarginHorizontal 8 10)
          , imageWithFallback $ fetchImage FF_ASSET driverImage
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text ((DS.take 12 item.goodName) <> (if DS.length item.goodName > 12 then "..." else "")) 
          , textSize FontSize.a_14
          , color if aboveThreshold || (item == currentDriverData && currentDriverData.rank > 0) then Color.white900 else Color.black800
          , visibility $ if checkDriverWithZeroRides item aboveThreshold state then GONE else VISIBLE 
          ]
        ]
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity $ if checkDriverWithZeroRides item aboveThreshold state then LEFT else RIGHT
        , padding $ PaddingLeft (if checkDriverWithZeroRides item aboveThreshold state then 10 else 0 )
        , weight 1.0
        ][ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , text $ if checkDriverWithZeroRides item aboveThreshold state then  getStringEnToHi ACCEPT_RIDES_TO_ENTER_RANKINGS else (show item.rides) <> " " <> (getStringEnToHi RIDES)
          , textSize FontSize.a_16
          , fontStyle  $ FontStyle.semiBold LanguageStyle
          , color if aboveThreshold || (item == currentDriverData && currentDriverData.rank > 0) then Color.white900 else Color.black800
          ]
        ]
    ]

topRankers :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
topRankers state =
  let rankersList = state.props.rankersData
      personDetail1 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 0)
      personDetail2 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 1)
      personDetail3 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 2)
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin (Margin 16 15 16 15)
    , gradient (Linear 0.0 [Color.lightGradientBlue, Color.darkGradientBlue])
    , orientation HORIZONTAL
    , gravity CENTER
    , padding (PaddingVertical 20 24)
    , cornerRadius 16.0
    , visibility if not state.props.showShimmer then VISIBLE else GONE
    ][ rankers 72 2 Color.darkMint false FontSize.a_16 personDetail2  (imageWrtGenderRank personDetail2.gender personDetail2.rank)
    , rankers 102 1 Color.yellow900 true FontSize.a_22 personDetail1 (imageWrtGenderRank personDetail1.gender  personDetail1.rank)
    , rankers 72 3 Color.orange900 false FontSize.a_16 personDetail3 (imageWrtGenderRank personDetail3.gender  personDetail3.rank) 
    ]

imageWrtGenderRank :: String -> Int -> String
imageWrtGenderRank gender rank =
  case gender , rank  of
    "MALE", 1 -> "ny_ic_rank1"
    "MALE", 2 -> "ny_ic_rank2"
    "MALE", 3 -> "ny_ic_rank3"
    "FEMALE", 1 -> "ny_ic_rank1_female"
    "FEMALE", 2 -> "ny_ic_rank2_female"
    "FEMALE", 3 -> "ny_ic_rank3_female"
    _,  _ -> "ny_ic_generic_mascot"

rankers :: forall w . Int -> Int -> String -> Boolean -> Font.FontSize -> ST.RankCardData  -> String -> PrestoDOM (Effect Unit) w
rankers size rank themeColor showCrown fontSize detail imageUrl =
  let bottomMargin = ceil ( (toNumber size) / 7.0 )
      rankWidth = bottomMargin * 2 + 5
  in
    linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , weight 1.0
    , gravity CENTER
    , orientation VERTICAL
    , margin (MarginHorizontal 10 10)
    ][ frameLayout
       [ width (V $ size)
       , height (V $ size + bottomMargin + 30)
       ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_HORIZONTAL
          ][ imageView
             [ height (V 31)
             , width (V 31)
             , imageWithFallback $ fetchImage FF_ASSET "ny_ic_crown"
             , visibility if rank == 1 then VISIBLE else GONE
             , padding (PaddingBottom 5)
             ]
           ]
        , linearLayout
          [ width (V $ size)
          , height (V $ size)
          , background Color.white900
          , cornerRadius (toNumber size)
          , stroke ("2," <> themeColor)
          , margin (MarginTop 28)
          ][ imageView
             [ width MATCH_PARENT
             , height MATCH_PARENT
             , imageWithFallback $ fetchImage FF_ASSET imageUrl
             ]
           ]
        , linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity BOTTOM
          ][ linearLayout
             [ width MATCH_PARENT
             , height (V $ bottomMargin * 2 + 3)
             , gravity CENTER
             ][ textView
                [ width (V $ rankWidth)
                , height MATCH_PARENT
                , text $ if detail.rank > 0 then show rank else "-"
                , textSize fontSize
                , fontStyle  $ FontStyle.bold LanguageStyle
                , color if rank ==  1 then Color.black900 else Color.white900
                , background themeColor
                , gravity CENTER
                , cornerRadius 14.0
                ]
              ]
           ]
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , orientation VERTICAL
        ][ textView
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , text $ (show detail.rides) <> " " <> (getStringEnToHi RIDES)
           , gravity CENTER
           , textSize FontSize.a_18
           , fontStyle  $ FontStyle.bold LanguageStyle
           , color Color.black800
           ]
         , textView
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , text $ if DS.length detail.goodName > 20 then (DS.take 20 detail.goodName) <> "..." else detail.goodName
           , gravity CENTER
           , textSize FontSize.a_14
           , color Color.black700
           ]
         ]
     ]

dateSelector :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
dateSelector push state =
  let leaderBoardType = state.props.leaderBoardType
  in
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    , visibility $ boolToVisibility state.props.showDateSelector
    , onClick push (const $ DateSelectorAction)
    ][horizontalScrollView
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , background Color.white900
      , cornerRadii $ Corners 18.0 false false true true
      , stroke $ "1," <> Color.grey900
      , scrollBarX false
      , id (getNewIDWithTag "DateSelector")
      ][  linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , padding (Padding 12 18 12 20)
          ] $ 
            case leaderBoardType of
              ST.Daily -> mapWithIndex dailyDateSelectorView state.props.days
              ST.Weekly -> mapWithIndex weeklyDateSelectorView state.props.weeks
              ST.Monthly -> mapWithIndex monthlyDateSelectorView state.props.months
      ]
    ]
  where
    dailyDateSelectorView :: forall w . Int -> CT.CalendarDate -> PrestoDOM (Effect Unit) w
    dailyDateSelectorView index item =
      let isSelected = item == state.props.selectedDay
          selectorText = show item.date
          maybeSuffixText = Just item.month
      in commonDateSelectorView (V 50) (V 50) VERTICAL isSelected selectorText maybeSuffixText $ ChangeDate $ ST.DaySelector item
    
    weeklyDateSelectorView :: forall w . Int -> CT.CalendarWeek -> PrestoDOM (Effect Unit) w
    weeklyDateSelectorView index item =
      let isSelected = item == state.props.selectedWeek
          selectorText = show item.startDate <> " " <> item.startMonth <> " - " <> show item.endDate <> " " <> item.endMonth
      in commonDateSelectorView (V 140) (V 50) HORIZONTAL isSelected selectorText Nothing $ ChangeDate $ ST.WeekSelector item
    
    monthlyDateSelectorView :: forall w . Int -> CT.CalendarMonth -> PrestoDOM (Effect Unit) w
    monthlyDateSelectorView index item =
      let isSelected = item == state.props.selectedMonth
          selectorText = convertUTCtoISC item.utcDate "MMMM"
      in commonDateSelectorView (V 140) (V 50) HORIZONTAL isSelected selectorText Nothing $ ChangeDate $ ST.MonthSelector item

    commonDateSelectorView :: forall w . Length -> Length -> Orientation -> Boolean -> String -> Maybe String -> Action -> PrestoDOM (Effect Unit) w
    commonDateSelectorView layoutWidth layoutheight layoutOrientation isSelected selectorText maybeSuffixText action =
      linearLayout
      [ width layoutWidth
      , height layoutheight
      , cornerRadius 50.0
      , background if isSelected then Color.blue600 else Color.grey700
      , margin $ MarginHorizontal 4 4
      , gravity CENTER
      , orientation layoutOrientation
      , stroke $ "1," <> if isSelected then Color.blue800 else Color.grey700
      , onClick push $ const action
      ]
      [ textView $
        [ text selectorText
        , color if isSelected then Color.blue800 else Color.black700
        ] <> FontStyle.body32 LanguageStyle
      , textView $
        [ text $ fromMaybe "" maybeSuffixText
        , visibility $ boolToVisibility $ isJust maybeSuffixText
        , color if isSelected then Color.blue800 else Color.black700
        ] <> FontStyle.body16 LanguageStyle
      ]

leaderBoardTab :: forall w . String -> ST.LeaderBoardType -> (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoardTab _text leaderBoardType push state =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if leaderBoardType == state.props.leaderBoardType then Color.black900 else Color.grey800
  , padding (Padding 8 8 8 8)
  , weight 1.0
  , gravity CENTER
  , color if leaderBoardType == state.props.leaderBoardType then Color.white900 else Color.black700
  , cornerRadius 20.0
  , textSize FontSize.a_14
  , onClick (\action ->
              if state.props.leaderBoardType /= leaderBoardType then do
                _ <- push action
                pure unit
              else pure unit
            ) (const $ ChangeLeaderBoardtab leaderBoardType)
  ]

bottomNavBarView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.REFERRAL_SCREEN state.data.config.bottomNavConfig)]


commonView :: forall w . (Action -> Effect Unit) -> String -> String -> String -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
commonView push img title description state=
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
                        if state.props.stage == ST.SuccessScreen then do
                          void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do
                            liftEffect $ startTimer state.props.seconds state.props.id "1" push SuccessScreenExpireCountDwon
                        else pure unit
                        push action
                  ) (const SuccessScreenRenderAction)
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , padding (PaddingHorizontal 16 16)
        , weight 1.0
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ imageView
              [ height $ if img == "ny_ic_comming_soon_poster" then V 290 else V 112
              , width $ if img == "ny_ic_comming_soon_poster" then V 234 else V 112
              , imageUrl img
              , margin $ if img == "ny_ic_comming_soon_poster" then (Margin 0 0 0 0) else (MarginBottom 72)
              , onClick push (const if img == "ny_ic_comming_soon_poster" then EnableReferralFlow else EnableReferralFlowNoAction)
              ]
          , textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text title
              , color Color.black900
              ] <> FontStyle.h2 LanguageStyle
          , textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text description
              , color Color.black700
              , padding (PaddingVertical 10 10)
              ] <> FontStyle.paragraphText TypoGraphy
          ]
        ]
    ]


referralEnrolmentFlow :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referralEnrolmentFlow push state =
    scrollView
    [
      width MATCH_PARENT
    , weight 1.0
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , height WRAP_CONTENT
        , weight 1.0
        , padding $ Padding 16 16 16 16
        ]
        [ textView (
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text (getString DRIVER_DETAILS)
          , textSize $ FontSize.a_14
          , color Color.greyTextColor
          , alpha 0.8
          ] <> FontStyle.tags TypoGraphy)
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.grey700
          , cornerRadius 5.0
          , margin $ MarginTop 8
          , padding $ Padding 16 15 0 16
          ]
          [
            textView (
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text state.data.driverInfo.driverName
            , color Color.black800
            , textSize $ FontSize.a_14
            ] <> FontStyle.body1 TypoGraphy)
          , textView (
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ "+91 "<> (fromMaybe "" state.data.driverInfo.driverMobile)
            , color Color.black800
            , textSize $ FontSize.a_16
            , margin (MarginVertical 8 8)
            ] <> FontStyle.body1 TypoGraphy)
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , padding $ Padding 3 3 3 3
            , background Color.golden
            , cornerRadius 4.0
            , gravity CENTER
            ]
            [
              linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              , padding $ Padding 7 4 7 4
              , background Color.golden
              , cornerRadius 4.0
              , stroke "1,#454545"
              , gravity CENTER
              ]
              [
                textView (
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text state.data.driverInfo.vehicleRegNumber
                , color Color.black800
                , textSize $ FontSize.a_16
                ] <> FontStyle.subHeading1 TypoGraphy)
              ]
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 10)
          ][  PrimaryEditText.view(push <<< PrimaryEditTextAction1) ({
              title: (getString REFERRAL_CODE_NUMBER)
              , hint: (getString REFERRAL_CODE_HINT)
              , valueId: ""
              , isinValid: false
              , error: Just (getString INVALID_MOBILE_NUMBER)
              , pattern : Just "[0-9]*,6"
              , text: ""
              , letterSpacing: PX 0.0
              , id: (getNewIDWithTag "EnterReferralCodeEditText")
              , fontSize : FontSize.a_18
              , type : "number"
              , background : state.data.config.themeColors.radioInactiveBackground
              , stroke : state.data.config.themeColors.editTextNormalStroke
            })
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 10)
          ][  PrimaryEditText.view(push <<< PrimaryEditTextAction2) ({
              title: (getString CONFIRM_REFERRAL_CODE)
              , hint: (getString CONFIRM_REFERRAL_CODE_HINT)
              , valueId: ""
              , isinValid: false
              , error: Just (getString INVALID_MOBILE_NUMBER)
              , pattern : Just "[0-9]*,6"
              , text: ""
              , letterSpacing: PX 0.0
              , id: (getNewIDWithTag "EnterConfirmReferralCoderEditText")
              , fontSize : FontSize.a_18
              , type : "number"
              , background : state.data.config.themeColors.radioInactiveBackground
              , stroke : state.data.config.themeColors.editTextNormalStroke
            })
          ]
        ]
      ]

continueButtonView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
continueButtonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , padding $ Padding 16 0 16 16
    ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]

qrScreenView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
qrScreenView push state =
  scrollView
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  ][linearLayout
    [ weight 1.0
    , width MATCH_PARENT
    , orientation VERTICAL
    ][ qrShimmerView push state
     , qrScreen push state
     ]  
   ]

qrShimmerView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
qrShimmerView push state = 
  let referallCodeViewTopMargin = 24
      referallCodeViewHeight = 516
      alertViewHeight = 80
      alertViewTopMargin = referallCodeViewTopMargin + referallCodeViewHeight + 16
      referredViewHeight = 84
      referredViewTopMargin = alertViewTopMargin + alertViewHeight + 16
  in
  shimmerFrameLayout
  [ width $ MATCH_PARENT
  , height $ MATCH_PARENT
  , orientation VERTICAL
  , visibility $ if state.props.showShimmer then VISIBLE else GONE
  , margin $ Margin 20 0 20 16
  ][ linearLayout
     [ width MATCH_PARENT
     , height $ V referallCodeViewHeight
     , background Color.greyDark
     , cornerRadius 12.0
     , margin $ MarginTop referallCodeViewTopMargin
     ][]
   , linearLayout
     [ width MATCH_PARENT
     , height $ V alertViewHeight
     , background Color.greyDark
     , cornerRadius 12.0
     , margin $ MarginTop alertViewTopMargin
     ][]
   , linearLayout
     [ width MATCH_PARENT
     , height $ V referredViewHeight
     , background Color.greyDark
     , cornerRadius 12.0
     , margin $ MarginTop referredViewTopMargin
     ][]
  ]

qrScreen :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
qrScreen push state =
  linearLayout
  [ weight 1.0
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 20 0 20 16)
  , visibility if not state.props.showShimmer then VISIBLE else GONE
  ][  linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.yellow900
      , cornerRadius 12.0
      , margin $ MarginTop 24
      , padding (PaddingBottom 5)
      ]
      [
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , margin $ MarginTop 20
          ]
          [
            imageView
              [ height $ V 49
              , width $ V 120
              , imageWithFallback $ getReferralScreenIcon (getMerchant FunctionCall) 
              , id $ getNewIDWithTag "ReferralScreenLogo"
              ]
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          , margin $ MarginTop 16
          ]
          [
            imageView
              [ height $ V 288
              , width $ V 288
              , id $ getNewIDWithTag "ReferralQRScreen"
              , afterRender push (const (ReferralQrRendered $ getNewIDWithTag "ReferralQRScreen"))
              ]
          ]
      , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , text (getString YOUR_REFERRAL_CODE)
        , color Color.black900
        , textSize $ FontSize.a_16
        , margin $ MarginTop 8
        , visibility $ if isJust state.data.driverInfo.referralCode then VISIBLE else GONE
        ]
      , textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , text (fromMaybe "" state.data.driverInfo.referralCode)
        , color Color.black900
        , margin $ MarginTop 4
        ] <> FontStyle.h4 TypoGraphy 
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity CENTER
        , visibility VISIBLE
        , onClick push (const ShareOptions)
        , padding (Padding 4 12 4 12)
        ][ linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , cornerRadius 24.0
            , background Color.white900
            , orientation HORIZONTAL
            ]
            [ imageView
              [ height $ V 30
              , width $ V 30
              , padding (PaddingLeft 10)
              , imageWithFallback $ fetchImage FF_ASSET "ny_ic_share_grey"
              ]
            , textView
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , textSize $ FontSize.a_12
              , color Color.black900
              , padding (Padding 0 6 10 0)
              , text  (" " <> getString SHARE_OPTIONS <> " ")
              ]
            ]
        ]

      ]
  ,   linearLayout
      [  height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 16
      , cornerRadius 12.0
      , background if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then Color.greenGrey else Color.black800
      ]
      [  linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , weight 1.0
          , padding $ Padding 20 13 0 0
          , orientation VERTICAL
          ]
          [ textView $
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity LEFT
            , text if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then (getString FIRST_REFERRAL_SUCCESSFUL) else (getString AWAITING_REFERRAL_RIDE)
            , color Color.white900
            ] <> FontStyle.body6 TypoGraphy
          , textView
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity LEFT
            , text (getString CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT)
            , color Color.white900
            , visibility if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then GONE else VISIBLE
            , textSize $ FontSize.a_12
            , fontStyle  $ FontStyle.regular LanguageStyle
            ]
          , contactUsTextView  push state
          ]
      ,   imageView
          [
            height $ V 80
          ,  width $ V 118
          , layoutGravity "bottom"
          , imageWithFallback $ if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then getActiveReferralBannerIcon state.data.driverInfo.vehicleVariant else getReferralBannerIcon state.data.driverInfo.vehicleVariant
          ]
      ]
  ,   linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginTop 16
      , background Color.grey700
      , cornerRadius 12.0
      , padding $ Padding 16 17 16 17
      , orientation VERTICAL
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [
          textView
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , weight 1.0
          , gravity LEFT
          , text (getString REFERRED_CUSTOMERS)
          , color Color.black800
          , textSize $ FontSize.a_14
          ]
        , textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , alignParentRight "true,-1"
          , gravity CENTER
          , text (show state.data.driverPerformance.referrals.totalReferredCustomers)
          , color Color.black800
          ] <> FontStyle.h2 LanguageStyle
        ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [
            textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , gravity LEFT
            , weight 1.0
            , text (getString ACTIVATED_CUSTOMERS)
            , color Color.black800
            ] <> FontStyle.paragraphText TypoGraphy
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , alignParentRight "true,-1"
            , gravity CENTER
            , text (show state.data.driverPerformance.referrals.totalActivatedCustomers)
            , color Color.black800
            ] <> FontStyle.h2 LanguageStyle
          ]
      ]
  ]

passwordPopUpView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
passwordPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , visibility if state.props.passwordPopUpVisible then VISIBLE else GONE
  , orientation VERTICAL
  ][PopUpModal.view (push <<< PasswordModalAction) (passwordPopUpConfig state )]

customerSupportPopUpView :: forall w. ST.ReferralScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerSupportPopUpView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.props.callSupportPopUpVisible then VISIBLE else GONE
  ][PopUpModal.view (push <<< ContactSupportAction) (contactSupportConfig state)]



contactUsTextView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
contactUsTextView push state =
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 4
  , visibility if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then VISIBLE else GONE
  , onClick push $ const GoToAlertScreen
  , padding $ PaddingBottom 4
  ][ textView $
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity LEFT
    , textFromHtml $ "<u>"<>(getString FOR_UPDATES_SEE_ALERTS) <> "</u>"
    , color Color.white900
    ] <> FontStyle.body3 TypoGraphy
  ]


emptyView :: forall w . PrestoDOM (Effect Unit) w
emptyView =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ][]

checkDriverWithZeroRides :: ST.RankCardData -> Boolean -> ST.ReferralScreenState -> Boolean
checkDriverWithZeroRides item aboveThreshold state = 
  let currentDriverData = state.props.currentDriverData
  in aboveThreshold && (item == currentDriverData && currentDriverData.rank == 0) 

checkDate :: ST.ReferralScreenState -> Boolean
checkDate state = if state.props.leaderBoardType == ST.Daily then (getcurrentdate "") == (convertUTCtoISC state.props.selectedDay.utcDate "YYYY-MM-DD") 
                  else (getcurrentdate "") <= (convertUTCtoISC state.props.selectedWeek.utcEndDate "YYYY-MM-DD") 

getReferralScreenIcon :: Merchant -> String
getReferralScreenIcon merchant = fetchImage FF_ASSET $
  case merchant of
    NAMMAYATRI -> "ny_namma_yatri"
    YATRI -> "ny_ic_yatri_logo_dark"
    YATRISATHI -> "ny_ic_yatri_sathi_logo_black_icon"
    _ -> "ny_namma_yatri"

getActiveReferralBannerIcon :: String -> String
getActiveReferralBannerIcon vehicleType = fetchImage FF_ASSET $
  case vehicleType of
    "AUTO_RICKSHAW" -> "ny_ic_auto2"
    _ -> "ny_ic_car_referral_banner"

getReferralBannerIcon :: String -> String
getReferralBannerIcon vehicleType = fetchImage FF_ASSET $
  case vehicleType of
    "AUTO_RICKSHAW" -> "ny_ic_auto1"
    _ -> "ny_ic_car_referral_banner"