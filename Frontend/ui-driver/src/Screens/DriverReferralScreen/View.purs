module Screens.DriverReferralScreen.View where

import Animation as Anim
import Effect (Effect)
import Prelude
import PrestoDOM
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.DriverReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types
import Screens.DriverReferralScreen.ComponentConfig
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Helpers.Utils
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Screens as ScreenNames
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Services.API
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Debug (spy)
import Mobility.Prelude
import Engineering.Helpers.BackTrack (liftFlowBT)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Maybe (isJust, fromMaybe)

screen :: DriverReferralScreenState -> Screen Action DriverReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverReferralScreen"
  , globalEvents:
      [ ( \push -> do
            _ <-
              launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    (GetPerformanceRes referralInfoResp) <- Remote.getPerformanceBT (GetPerformanceReq {})
                    lift $ lift $ doAff do liftEffect $ push $ UpdateDriverPerformance (GetPerformanceRes referralInfoResp)
                    (LeaderBoardRes leaderBoardResp) <- Remote.leaderBoardBT $ DailyRequest (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                    lift $ lift $ doAff do liftEffect $ push $ UpdateLeaderBoard (LeaderBoardRes leaderBoardResp)
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "DriverReferralScreen ----- state" state
          let
            _ = spy "DriverReferralScreen --------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , onBackPressed push $ const BackPressed
    , afterRender push $ const AfterRender
    , background Color.white900
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , orientation VERTICAL
            , weight 1.0
            , height WRAP_CONTENT
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                ]
                [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state) ]
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ Margin 16 0 16 12
                , orientation VERTICAL
                ]
                [ if shouldShowReferral state then driverReferralCode push state else dummyView
                , rideLeaderBoardView push state
                ]
            ]
        , bottomNavBarView push state
        ]
    , if state.props.showDriverReferralQRCode then appQRCodeView push state else dummyView
    , if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then referralInfoPop push state else dummyView
    ]

tabView :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
tabView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 24.0
    , stroke $ "1," <> strokeColor
    , background backgroundColor
    , padding $ Padding 2 2 2 2
    , margin $ MarginBottom 16
    , gravity CENTER
    ]
    [ tabItem push (state.props.driverReferralType == DRIVER) (getString REFER_DRIVER) "ny_ic_new_avatar_profile" DRIVER bothTabsEnabled $ cityConfig.showDriverReferral || state.data.config.enableDriverReferral
    , tabItem push (state.props.driverReferralType == CUSTOMER) (getString REFER_CUSTOMER) "ny_ic_new_avatar_profile_customer" CUSTOMER bothTabsEnabled $ cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
    ]
  where
  cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

  bothTabsEnabled = (cityConfig.showDriverReferral || state.data.config.enableDriverReferral) && (cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral)

  backgroundColor = if bothTabsEnabled then Color.grey800 else Color.transparent

  strokeColor = if bothTabsEnabled then Color.grey900 else Color.transparent

tabItem :: forall w. (Action -> Effect Unit) -> Boolean -> String -> String -> DriverReferralType -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
tabItem push isActive text' img referralType bothTabsEnabled visibility' =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , padding $ PaddingVertical 6 8
    , weight 1.0
    , background tabBackground
    , gravity CENTER
    , cornerRadius 24.0
    , onClick push $ const $ ChangeTab referralType
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET img
        , height $ V 24
        , width $ V 24
        , margin $ MarginRight 12
        ]
    , textView
        $ [ height WRAP_CONTENT
          , text text'
          , color if isActive && bothTabsEnabled then Color.white900 else Color.black800
          , padding $ PaddingBottom 3
          ]
        <> FontStyle.tags TypoGraphy
    ]
  where
  tabBackground = case bothTabsEnabled, isActive of
    true, true -> Color.black900
    true, false -> Color.grey800
    false, _ -> Color.black80

shouldShowReferral :: DriverReferralScreenState -> Boolean
shouldShowReferral state =
  let
    cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

    driverReferral = cityConfig.showDriverReferral || state.data.config.enableDriverReferral

    customerReferral = cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
  in
    driverReferral || customerReferral

driverReferralCode :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
driverReferralCode push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 20 16 20 16
    , background config.backgroundColor
    , margin $ MarginBottom 32
    , cornerRadius 12.0
    , gravity CENTER
    ]
    [ tabView push state
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width $ V 148
            , background Color.white900
            , orientation VERTICAL
            , gravity CENTER
            , cornerRadius 10.0
            ]
            [ imageView
                [ width $ V 148
                , height $ V 148
                , gravity CENTER
                , imageWithFallback $ fetchImage FF_ASSET config.qr_img
                ]
            , textView
                $ [ width MATCH_PARENT
                  , gravity CENTER
                  , text $ getString CLICK_TO_EXPAND
                  , onClick push $ const ShowQRCode
                  , color Color.black800
                  , background Color.blue600
                  , padding $ PaddingBottom 2
                  , cornerRadii $ Corners 6.0 false false true true
                  ]
                <> FontStyle.body3 TypoGraphy
            ]
        , linearLayout
            [ height MATCH_PARENT
            , weight 1.0
            , gravity CENTER
            , orientation VERTICAL
            , padding $ PaddingLeft 16
            ]
            [ textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ getString config.referralText
                  , color Color.black900
                  , gravity CENTER
                  ]
                <> FontStyle.paragraphText TypoGraphy
            , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , gravity CENTER
                , text state.data.referralCode
                , color Color.black900
                , fontStyle $ FontStyle.feFont LanguageStyle
                , textSize FontSize.a_30
                , margin $ MarginTop 10
                ]
            ]
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background config.separatorColor
        , margin $ MarginTop 10
        ]
        []
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 10
        ]
        [ referralCountView false (getString REFERRED) (show state.data.totalReferredCustomers) (state.props.driverReferralType == CUSTOMER) push REFERRED_CUSTOMERS_POPUP
        , referralCountView true config.infoText (show activatedCount) true push config.popupType
        ]
    ]
  where
  activatedCount = if state.props.driverReferralType == DRIVER then state.data.totalReferredDrivers else state.data.totalActivatedCustomers

  config = if state.props.driverReferralType == DRIVER then driverReferralConfig else customerReferralConfig

  driverReferralConfig =
    { backgroundColor: Color.yellow900
    , qr_img: "ny_driver_app_qr_code"
    , infoText: getString REFERRED_DRIVERS
    , separatorColor: Color.white300
    , popupType: REFERRED_DRIVERS_POPUP
    , referralText: DRIVER_REFERRAL_CODE
    }

  customerReferralConfig =
    { backgroundColor: Color.frenchSkyBlue800
    , qr_img: "ny_customer_app_qr_code"
    , infoText: getString ACTIVATED
    , separatorColor: Color.frenchSkyBlue400
    , popupType: ACTIVATED_CUSTOMERS_POPUP
    , referralText: CUSTOMER_REFERRAL_CODE
    }

referralCountView :: forall w. Boolean -> String -> String -> Boolean -> (Action -> Effect Unit) -> ReferralInfoPopType -> PrestoDOM (Effect Unit) w
referralCountView showStar text' count visibility' push popupType =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , weight 1.0
    , visibility $ boolToVisibility visibility'
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_star_black"
              , height $ V 13
              , width $ V 13
              , margin $ MarginRight 4
              , visibility $ boolToVisibility showStar
              ]
        , textView
            $ [ height WRAP_CONTENT
              , text text'
              , color Color.black800
              ]
            <> FontStyle.tags TypoGraphy
        , imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
              , height $ V 14
              , width $ V 14
              , margin $ Margin 4 2 0 0
              , onClick push $ const $ ShowReferedInfo popupType
              ]
        ]
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text count
          , color Color.black800
          , weight 1.0
          , gravity countGravity
          ]
        <> FontStyle.body6 TypoGraphy
    ]
  where
  countGravity = if showStar then RIGHT else CENTER

appQRCodeView :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
appQRCodeView push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    , clickable true
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , cornerRadius 16.0
        , background state.data.config.popupBackground
        , margin $ MarginHorizontal 10 10
        , padding $ Padding 24 12 24 12
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , text $ getString $ DOWNLOAD_NAMMA_YATRI "DOWNLOAD_NAMMA_YATRI"
              , margin $ MarginVertical 10 7
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        , imageView
            [ width MATCH_PARENT
            , height $ V 280
            , gravity CENTER
            , imageWithFallback $ fetchImage FF_ASSET qr_img
            ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
        ]
    ]
  where
  qr_img = if state.props.driverReferralType == DRIVER then "ny_driver_app_qr_code" else "ny_customer_app_qr_code"

referralInfoPop :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
referralInfoPop push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , cornerRadius 16.0
        , background state.data.config.popupBackground
        , margin $ MarginHorizontal 10 10
        , padding $ Padding 24 12 24 12
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , text $ config.heading
              , margin $ MarginVertical 10 7
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER
              , text $ config.subtext
              , margin $ MarginVertical 10 7
              , color Color.black700
              ]
            <> FontStyle.paragraphText TypoGraphy
        , textView
            $ [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity CENTER
              , text $ getString GOT_IT
              , onClick push $ const $ ShowReferedInfo NO_REFERRAL_POPUP
              , margin $ MarginVertical 10 7
              , color Color.blue800
              ]
            <> FontStyle.subHeading1 TypoGraphy
        ]
    ]
  where
  qr_img = if state.props.driverReferralType == DRIVER then "ny_driver_app_qr_code" else "ny_customer_app_qr_code"

  config = case state.props.referralInfoPopType of
    REFERRED_DRIVERS_POPUP -> { heading: getString REFERRED_DRIVERS, subtext: getString $ REFERRED_DRIVERS_INFO "REFERRED_DRIVERS_INFO" }
    REFERRED_CUSTOMERS_POPUP -> { heading: getString REFERRED_CUSTOMERS, subtext: getString $ REFERRED_CUSTOMERS_INFO "REFERRED_CUSTOMERS_INFO" }
    ACTIVATED_CUSTOMERS_POPUP -> { heading: getString ACTIVATED_CUSTOMERS, subtext: getString ACTIVATED_CUSTOMERS_INFO }
    _ -> { heading: "", subtext: "" }

rideLeaderBoardView :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
rideLeaderBoardView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ text $ getString RIDE_LEADERBOARD
          , color Color.black800
          , margin $ MarginBottom 12
          , visibility $ boolToVisibility (shouldShowReferral state)
          ]
        <> FontStyle.h2 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.greenGrey100
        , padding $ Padding 20 5 20 10
        , gravity CENTER_VERTICAL
        , cornerRadius 12.0
        ]
        [ imageView
            [ width $ V 77
            , height $ V 97
            , gravity CENTER
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_leaderboard"
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity LEFT
            , weight 1.0
            , margin $ MarginLeft 25
            ]
            [ textView
                $ [ text $ if driverNotInLBdOrLBNotReady then getString YOUR_DAILY_RANK else getString ACCEPT_RIDE_TO_ENTER_LEADERBOARD
                  , padding $ PaddingBottom 2
                  , color Color.green700
                  ]
                <> ( if driverNotInLBdOrLBNotReady then
                      FontStyle.body7 TypoGraphy
                    else
                      FontStyle.subHeading1 TypoGraphy
                  )
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , gravity CENTER
                , visibility $ boolToVisibility driverNotInLBdOrLBNotReady
                ]
                [ textView
                    $ [ text $ show $ fromMaybe 0 state.data.rank
                      , color Color.green700
                      ]
                    <> FontStyle.h0 TypoGraphy
                , textView
                    $ [ text $ " / " <> (formatEligibleDrivers $ fromMaybe 0 state.data.totalEligibleDrivers)
                      , color Color.green700
                      ]
                    <> FontStyle.body3 TypoGraphy
                ]
            ]
        , imageView
            [ width $ V 32
            , height $ V 32
            , gravity CENTER
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_arrow_right_circle_green"
            , onClick push $ const GoToLeaderBoard
            ]
        ]
    ]
  where
  driverNotInLBdOrLBNotReady = (isJust state.data.rank && isJust state.data.totalEligibleDrivers)

bottomNavBarView :: forall w. (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.REFERRAL_SCREEN state.data.config.bottomNavConfig) ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [ visibility GONE ] []

formatEligibleDrivers :: Int -> String
formatEligibleDrivers value
  | value > 10000 = show (value `div` 1000) <> "K"
  | otherwise = show value
