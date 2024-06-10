module Screens.Benefits.BenefitsScreen.View where

import Animation as Anim
import Effect (Effect)
import Prelude
import PrestoDOM
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Benefits.BenefitsScreen.Controller (Action(..), ScreenOutput, eval, getRemoteBannerConfigs)
import Screens.Types
import Screens.Benefits.BenefitsScreen.ComponentConfig
import Screens.Benefits.BenefitsScreen.ScreenData
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Helpers.Utils
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Screens as ScreenNames
import Language.Strings (getString, getStringEnToHi)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState, GlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Services.API
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (Flow,doAff)
import Effect.Class (liftEffect)
import Debug (spy)
import Mobility.Prelude
import Engineering.Helpers.BackTrack (liftFlowBT)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Maybe (isJust, fromMaybe, Maybe(..), maybe, isNothing)
import Effect.Uncurried (runEffectFn4)
import ConfigProvider
import Data.Int(fromNumber, toNumber, ceil)
import Data.Array(mapWithIndex, null)
import Animation as Anim
import Data.Array (length)
import Data.Either (Either(..))
import Locale.Utils
import PrestoDOM.Animation as PrestoAnim
import CarouselHolder as CarouselHolder
import Components.BannerCarousel as BannerCarousel
import PrestoDOM.List
import Engineering.Helpers.Commons as EHC
import Presto.Core.Flow (Flow)

screen :: BenefitsScreenState -> Screen Action BenefitsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "BenefitsScreen"
  , globalEvents:
      [ ( \push -> do
            _ <-
              launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    (GetPerformanceRes referralInfoResp) <- Remote.getPerformanceBT (GetPerformanceReq {})
                    lift $ lift $ doAff do liftEffect $ push $ UpdateDriverPerformance (GetPerformanceRes referralInfoResp)
                    (LeaderBoardRes leaderBoardResp) <- Remote.leaderBoardBT $ DailyRequest (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                    lift $ lift $ doAff do liftEffect $ push $ UpdateLeaderBoard (LeaderBoardRes leaderBoardResp)
            void $ launchAff $ flowRunner defaultGlobalState do
                moduleResp <- Remote.getAllLmsModules (getLanguageTwoLetters $ Just (getLanguageLocale languageKey))
                case moduleResp of
                  Right modules -> liftFlow $ push $ UpdateModuleList modules
                  Left err -> liftFlow $ push $ UpdateModuleListErrorOccurred
            when (isNothing initialState.data.bannerData.bannerItem) $ void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
            when (isNothing initialState.data.carouselItem) $ void $ launchAff $ flowRunner defaultGlobalState $ computeReferralListItem push initialState
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "BenefitsScreen ----- state" state
          let
            _ = spy "BenefitsScreen --------action" action
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ]$[ PrestoAnim.animationSet [Anim.fadeIn true] $ 
     linearLayout
     [ width $ MATCH_PARENT
     , height $ MATCH_PARENT
     ][ referralScreenBody push state ]
  ] <> if state.props.showDriverReferralQRCode then [appQRCodeView push state] else []
    <> if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then [referralInfoPop push state] else []

referralScreenBody :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralScreenBody push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ MATCH_PARENT
  , orientation VERTICAL
  , padding $ PaddingBottom safeMarginBottom
  ][   linearLayout
       [ width $ MATCH_PARENT
       , weight 1.0
       , orientation VERTICAL
       ][ headerLayout state push
        , scrollView
          [ height $ WRAP_CONTENT
          , width MATCH_PARENT
          , scrollBarY false 
          ][referralScreenInnerBody push state]
       ]
    ,  bottomNavBarView push state
  ]

separatorView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 1
  , backgroundColor $ Color.green700
  ][]

referralScreenInnerBody :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralScreenInnerBody push state = 
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  ]([ if fromMaybe false state.props.isPayoutEnabled then getCarouselView else dummyView
    , GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ case state.data.carouselItem of 
          Just item -> if shouldShowReferral state then driverReferralCode push state item else dummyView
          Nothing -> dummyView
      , if state.data.config.leaderBoard.enable then rideLeaderBoardView push state else dummyView
      ]
    , learnAndEarnShimmerView push state
  ] <> if not (null state.data.moduleList.completed) || not (null state.data.moduleList.remaining) then [learnAndEarnView push state] else [])
  where
    getCarouselView = maybe (linearLayout[][]) (\item -> bannersCarousal item state push) state.data.bannerData.bannerItem

-------------------------------------------------- headerLayout --------------------------
headerLayout :: BenefitsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerLayout state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding $ Padding 10 (safeMarginTopWithDefault 13) 10 13
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (case bothReferralNotEnabled of
                        true -> getString RIDE_LEADERBOARD
                        false -> getString REFERRAL)
              , margin $ MarginLeft 10
              , padding $ PaddingBottom 2
              , color Color.black900
              ]
            <> FontStyle.h3 TypoGraphy
        ]
    ]
  where
    cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION) 
    bothReferralNotEnabled = not (cityConfig.showDriverReferral || state.data.config.enableDriverReferral || cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral)

tabView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
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
    [ tabItem push (state.props.driverReferralType == CUSTOMER) (getString REFER_CUSTOMER) "ny_ic_new_avatar_profile_customer" CUSTOMER bothTabsEnabled $ cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
    ,  tabItem push (state.props.driverReferralType == DRIVER) (getString REFER_DRIVER) "ny_ic_new_avatar_profile" DRIVER bothTabsEnabled $ cityConfig.showDriverReferral || state.data.config.enableDriverReferral
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

shouldShowReferral :: BenefitsScreenState -> Boolean
shouldShowReferral state =
  let
    cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

    driverReferral = cityConfig.showDriverReferral || state.data.config.enableDriverReferral

    customerReferral = cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
  in
    driverReferral || customerReferral

driverReferralCode :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> ListItem -> PrestoDOM (Effect Unit) w
driverReferralCode push state item =
  let
    isCustomer = state.props.driverReferralType == CUSTOMER
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ CarouselHolder.carouselView push $ getReferralCarouselConfig item state
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER
          ]
          [ indicatorDot (if isCustomer then Color.black900 else Color.grey900) isCustomer
          , indicatorDot (if isCustomer then Color.grey900 else Color.black900) isCustomer
          ]
      ]
  where indicatorDot color isCustomer = linearLayout
              [ height $ V 4
              , width $ V 4
              , margin $ (if isCustomer then MarginLeft else MarginRight) 2
              , cornerRadius 2.0
              , background color 
              ]
              []

getReferralCarouselConfig ∷ forall a. ListItem → BenefitsScreenState → CarouselHolder.CarouselHolderConfig CarouselProps Action
getReferralCarouselConfig view state = {
    view
  , items : referralCardArray state
  , orientation : VERTICAL
  , currentPage : 0
  , autoScroll : false
  , autoScrollDelay : 5000.0
  , id : "referralCarousel"
  , autoScrollAction : Nothing
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just ReferrlBannerChanged
  , onPageScrolled : Nothing
  , currentIndex : 0
  , showScrollIndicator : false
  , layoutHeight : V 300
  , overlayScrollIndicator : true
}

referralCardArray :: BenefitsScreenState -> Array (Record CarouselProps)
referralCardArray state = 
  let 
    titleCornerRadius = if os == "IOS" then "19.0" else "40.0"
    cornerRadius = if os == "IOS" then "10.0" else "20.0"
  in 
  [{
  title : toPropValue $ getString REFER_CUSTOMER,
  qrCode : toPropValue $ renderQrHolder $ Qr ((generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode CUSTOMER)) 500 0,
  titleBackground : toPropValue "#142C2F3A",
  background : toPropValue "#F1F2F7",
  titleImage : toPropValue "ny_ic_new_avatar_profile_customer",
  titleCornerRadius: toPropValue titleCornerRadius,
  cornerRadius: toPropValue cornerRadius,
  referralCode: toPropValue state.data.referralCode,
  bodyText : toPropValue $ getString CUSTOMER_REFERRAL_CODE,
  isDriver : toPropValue "gone",
  isCustomer : toPropValue "visible",
  referredCustomer : toPropValue $ show state.data.totalReferredDrivers,
  activatedCustomers : toPropValue $ show state.data.totalActivatedCustomers,
  referredDrivers : toPropValue $ show state.data.totalReferredDrivers
},{
  title : toPropValue $ getString REFER_DRIVER,
  qrCode : toPropValue $ renderQrHolder $ Qr ((generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode DRIVER)) 500 0,
  background : toPropValue "#E0D1FF",
  titleBackground : toPropValue "#142C2F3A",
  titleImage : toPropValue "ny_ic_new_avatar_profile",
  titleCornerRadius: toPropValue titleCornerRadius,
  cornerRadius: toPropValue cornerRadius,
  referralCode: toPropValue state.data.referralCode,
  bodyText : toPropValue $ getString DRIVER_REFERRAL_CODE,
  isCustomer : toPropValue "gone",
  isDriver : toPropValue "visible",
  referredCustomer : toPropValue $ show state.data.totalReferredDrivers,
  activatedCustomers : toPropValue $ show state.data.totalActivatedCustomers,
  referredDrivers : toPropValue $ show state.data.totalReferredDrivers
}]

referralCardHolder :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralCardHolder push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ Padding 20 16 20 16
        , cornerRadiusHolder "cornerRadius"
        , backgroundHolder "background"
        , margin $ Margin 16 0 16 5
        , gravity CENTER
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , padding $ PaddingVertical 6 8
            , width MATCH_PARENT
            , cornerRadiusHolder "titleCornerRadius"
            , backgroundHolder "titleBackground"
            , gravity CENTER
            ]
            [ imageView
                [ imageUrlHolder "titleImage"
                , height $ V 24
                , width $ V 24
                , margin $ MarginRight 12
                ]
            , textView
                $ [ height WRAP_CONTENT
                  , textHolder "title"
                  , color Color.black900
                  , padding $ PaddingBottom 3
                  ]
                <> FontStyle.tags TypoGraphy
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginTop 16
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width $ V 164
                , background Color.white900
                , orientation VERTICAL
                , gravity CENTER
                , cornerRadius 10.0
                , onClickHolder push $ ShowQRCode
                , padding $ PaddingTop 8
                ]
                [ imageView
                    [ width $ V 148
                    , height $ V 148
                    , gravity CENTER
                    , qrHolder "qrCode"
                    ]
                , textView
                    $ [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , gravity CENTER
                      , text $ getString CLICK_TO_EXPAND
                      , color Color.black800
                      , background Color.blue600
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
                [ imageView
                    [ width $ V 40
                    , height $ V 40
                    , gravity CENTER
                    , padding (Padding 5 5 5 5)
                    , imageUrlHolder "titleImage"
                    ]
                , textView
                    $ [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , textHolder "bodyText"
                      , color Color.black900
                      , gravity CENTER
                      ]
                    <> FontStyle.paragraphText TypoGraphy
                , textView
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , gravity CENTER
                    , textHolder "referralCode"
                    , color Color.black900
                    , fontStyle $ FontStyle.feFont LanguageStyle
                    , textSize FontSize.a_30
                    , margin $ MarginTop 10
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity CENTER_VERTICAL
                    , cornerRadius 12.0
                    , background Color.white900
                    , orientation HORIZONTAL
                    , margin $ MarginTop 12
                    , padding $ Padding 8 4 8 4
                    , onClickHolder push ShareQRLink
                    ]
                    [ imageView
                        [ height $ V 16
                        , width $ V 16
                        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_share_grey"
                        ]
                    , textView
                        $ [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , color Color.black900
                          , margin $ Margin 8 0 0 1
                          , gravity CENTER_VERTICAL
                          , text $ getString SHARE
                          ]
                        <> FontStyle.body1 TypoGraphy
                    ]
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height $ V 1
            , background Color.white160
            , margin $ MarginTop 10
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ MarginTop 10
            ]
            [ customerReferredView push state
            , driverReferredView push state
            ]
        ]
    ]


customerReferredView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
customerReferredView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , visibilityHolder "isCustomer"
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER_VERTICAL
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , text $ getString REFERRED
              , color Color.black800
              ]
            <> FontStyle.tags TypoGraphy
        , imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
              , height $ V 14
              , width $ V 14
              , margin $ Margin 4 2 0 0
              , onClickHolder push $ ShowReferedInfo REFERRED_CUSTOMERS_POPUP
              , padding $ PaddingBottom 2
              ]
        , linearLayout [weight 1.0][]
        , textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , textHolder "referredCustomer"
              , color Color.black800
              , weight 1.0
              ]
            <> FontStyle.body6 TypoGraphy
        ]
      ,   linearLayout
            [ height WRAP_CONTENT
            , gravity CENTER_VERTICAL
            , weight 1.0
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
                      ]
                , textView
                    $ [ height WRAP_CONTENT
                      , text $ getString ACTIVATED
                      , color Color.black800
                      ]
                    <> FontStyle.tags TypoGraphy
                , imageView
                    $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
                      , height $ V 14
                      , width $ V 14
                      , margin $ Margin 4 2 0 0
                      , onClickHolder push $ ShowReferedInfo ACTIVATED_CUSTOMERS_POPUP
                      , padding $ PaddingBottom 2
                      ]
                ]
            , textView
                $ [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , textHolder "activatedCustomers"
                  , color Color.black800
                  , weight 1.0
                  , gravity RIGHT
                  ]
                <> FontStyle.body6 TypoGraphy
            ]
    ]


shineAnimation :: forall w . BenefitsScreenState -> PrestoDOM (Effect Unit) w
shineAnimation state =
  linearLayout
  [ height $ MATCH_PARENT
  , width $ WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , clipChildren true
  , clipToPadding true
  ][ PrestoAnim.animationSet [ Anim.shimmerAnimation (-100) ((screenWidth unit) + 100) 2500] $ 
     linearLayout
     [ width $ V (screenWidth unit)
     , height $ MATCH_PARENT
     , gravity CENTER_VERTICAL
     ][linearLayout
       [ width $ V 10
       , height MATCH_PARENT
       , background Color.transparentWhite
       , rotation 20.0
       , cornerRadius 2.0
       , margin $ MarginHorizontal 10 6
       ][]
     , linearLayout
       [ width $ V 5
       , height MATCH_PARENT
       , background Color.transparentWhite
       , rotation 20.0
       , cornerRadius 2.0
       , margin $ MarginRight 20
       ][]
     ]
  ]

-- referralCountView :: forall w. Boolean -> String -> String -> Boolean -> (Action -> Effect Unit) -> ReferralInfoPopType -> PrestoDOM (Effect Unit) w
-- referralCountView showStar text' count visibility' push popupType =
driverReferredView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
driverReferredView push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , weight 1.0
    , visibilityHolder "isDriver"
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
              ]
        , textView
            $ [ height WRAP_CONTENT
              , text $ getString REFERRED_DRIVERS
              , color Color.black800
              ]
            <> FontStyle.tags TypoGraphy
        , imageView
            $ [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_black"
              , height $ V 14
              , width $ V 14
              , margin $ Margin 4 2 0 0
              , onClickHolder push $ ShowReferedInfo REFERRED_DRIVERS_POPUP
              , padding $ PaddingBottom 2
              ]
        ]
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , textHolder "referredDrivers"
          , color Color.black800
          , weight 1.0
          , gravity RIGHT
          ]
        <> FontStyle.body6 TypoGraphy
    ]

appQRCodeView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
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
            [ width $ V 280
            , height $ V 280
            , gravity CENTER
            , padding (Padding 5 5 5 5)
            , qr $ Qr (generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode state.props.driverReferralType) 280 0
            ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
        ]
    ]

referralInfoPop :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralInfoPop push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    , onClick push $ const $ ShowReferedInfo NO_REFERRAL_POPUP 0
    , visibility $ boolToVisibility $ state.props.referralInfoPopType /= NO_REFERRAL_POPUP
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
              , onClick push $ const $ ShowReferedInfo NO_REFERRAL_POPUP 0
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

rideLeaderBoardView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
rideLeaderBoardView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 16 0 16 12
    ]
    [ textView
        $ [ text $ getStringEnToHi RIDE_LEADERBOARD
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
        , onClick push $ const GoToLeaderBoard
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
                $ [ text $ if driverNotInLBdOrLBNotReady then getStringEnToHi YOUR_DAILY_RANK else getStringEnToHi ACCEPT_RIDE_TO_ENTER_LEADERBOARD
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
            ]
        ]
    ]
  where
  driverNotInLBdOrLBNotReady = (isJust state.data.rank && isJust state.data.totalEligibleDrivers)

bottomNavBarView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state = BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.REFERRAL_SCREEN state.data.config.bottomNavConfig)

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = linearLayout [ visibility GONE ] []

formatEligibleDrivers :: Int -> String
formatEligibleDrivers value
  | value > 10000 = show (value `div` 1000) <> "K"
  | otherwise = show value


learnAndEarnShimmerView :: forall w.(Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
learnAndEarnShimmerView push state =
  shimmerFrameLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , visibility $ if state.props.showShimmer then VISIBLE else GONE
  , margin $ Margin 16 16 16 16
  ][ linearLayout
     [ width $ MATCH_PARENT
     , background Color.greyDark
     , height $ V 50
     , cornerRadius $ 16.0
     ][]
  ,  linearLayout[
       width $ MATCH_PARENT
     , background Color.greyDark
     , height $ V 100
     , cornerRadius $ 16.0
     , margin $ MarginTop 66
     ][]
  ,  linearLayout[
       width $ MATCH_PARENT
     , background Color.greyDark
     , height $ V 100
     , cornerRadius $ 16.0
     , margin $ MarginTop 182
     ][]
  ]

learnAndEarnView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
learnAndEarnView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , padding $ PaddingHorizontal 16 16
  , visibility $ if (state.props.showShimmer && length (state.data.moduleList.remaining <> state.data.moduleList.completed) == 0) then GONE else VISIBLE
  ][ textView $
     [ text $ getString LEARN_AND_EARN
     , color $ Color.black800
     , margin $ MarginVertical 12 12
     ] <> FontStyle.h2 TypoGraphy
    , linearLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , orientation VERTICAL
      ](map (\moduleInfo -> moduleCardView push state moduleInfo) (state.data.moduleList.remaining <> state.data.moduleList.completed))
  ]

moduleCardView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> LmsModuleRes -> PrestoDOM (Effect Unit) w
moduleCardView push state (LmsModuleRes moduleInfo) =
  let sWidth = ((toNumber((screenWidth unit) - 32)) * 1.0) / (toNumber 328)
      sHeight = (toNumber 160) * sWidth
  in 
  relativeLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 12.0
  , stroke $ "1," <> Color.lightBlueTeal
  , margin $ MarginBottom 24
  , onClick push $ const $ OpenModule (LmsModuleRes moduleInfo)
  , clickable true
  ][ linearLayout
     [ width $ V ((screenWidth unit) - 32)
     , height $ WRAP_CONTENT
     , orientation VERTICAL
     , cornerRadius 12.0
     ][ imageView
        [ width $ MATCH_PARENT
        , height $ V $ (ceil sHeight) - 2
        , imageWithFallback $ "," <> moduleInfo.thumbnailImage
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding $ Padding 16 5 16 12
        , gravity CENTER
        ][ moduleTitleAndNumberOfVideoView push state (LmsModuleRes moduleInfo)] 
      ]
  ,  imageView
      [ width $ V 15
      , height $ V 15
      , margin $ Margin 15 15 15 15
      , imageWithFallback $ ",https://firebasestorage.googleapis.com/v0/b/jp-beckn-dev.appspot.com/o/reelData%2F9-91490_social-sharing-share-button-white-png.png?alt=media&token=a501ef86-eb85-4c95-bea1-492737a29244"
      , alignParentRight "true,-1"
      , visibility GONE
      ]
  ]

moduleTitleAndNumberOfVideoView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> LmsModuleRes -> PrestoDOM (Effect Unit) w
moduleTitleAndNumberOfVideoView push state (LmsModuleRes moduleInfo) =
  let moduleStatusInfo = getStatusForModule
  in
  linearLayout
  [ weight 1.0
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][ textView $
    [ text moduleInfo.name
    , color Color.black900
    ] <> FontStyle.body23 LanguageStyle
  , linearLayout
    [ width $ WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    ][ textView $
        [ text moduleStatusInfo.noOfVideosToDisplay
        , color Color.black700
        , margin $ moduleStatusInfo.moduleMargin
        ] <> FontStyle.body24 LanguageStyle
      , statusPillView push state moduleStatusInfo.moduleStatus (Margin 0 0 0 0)
    ]
  ]
  where
    getStatusForModule :: {moduleStatus :: String, moduleMargin :: Margin, noOfVideosToDisplay :: String}
    getStatusForModule = let zeroVideosLeft = moduleInfo.noOfVideos - moduleInfo.noOfVideosCompleted == 0
                         in case moduleInfo.moduleCompletionStatus of
                              MODULE_NOT_YET_STARTED -> {moduleStatus : "NEW", moduleMargin : MarginRight 5, noOfVideosToDisplay : show moduleInfo.noOfVideos <> " " <> getString VIDEOS}
                              MODULE_ONGOING -> {moduleStatus : "PENDING", moduleMargin : if zeroVideosLeft then MarginRight 0 else MarginRight 5, 
                                                noOfVideosToDisplay : if zeroVideosLeft then "" else show (moduleInfo.noOfVideos - moduleInfo.noOfVideosCompleted) <> " " <> getString VIDEOS}
                              MODULE_COMPLETED -> {moduleStatus : "COMPLETED", moduleMargin : MarginRight 0, noOfVideosToDisplay : ""}

statusPillView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> String -> Margin -> PrestoDOM (Effect Unit) w
statusPillView push state status pillMargin =
  let pillProperty = getPropertyAccordingToStatus
  in
  linearLayout
  [ width $ WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , padding $ Padding 4 2 4 2
  , gravity CENTER
  , background pillProperty.pillBackgroundColor
  , cornerRadius pillProperty.cornerRadius
  , margin $ pillMargin
  ][ imageView
     [ width $ V 10
     , height $ V 10
     , visibility $ if pillProperty.shouldImageBeVisible then VISIBLE else GONE
     , imageWithFallback $ fetchImage FF_ASSET pillProperty.pillImage
     , margin $ MarginRight 3]
  ,  textView $
     [ text $ pillProperty.text
     , color $ pillProperty.textColor
     ] <> pillProperty.fontStyle
  ]
  where
    getPropertyAccordingToStatus :: {text :: String, textColor :: String, fontStyle :: forall properties. (Array (Prop properties)), cornerRadius :: Number, shouldImageBeVisible :: Boolean, pillBackgroundColor :: String, pillImage :: String}
    getPropertyAccordingToStatus = case status of
      "COMPLETED" -> {text : getString COMPLETED_STR, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, cornerRadius : 16.0, shouldImageBeVisible : true, pillBackgroundColor : Color.green900, pillImage : "ny_ic_white_tick"} 
      "PENDING" -> {text : getString PENDING_STR_C, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle,  cornerRadius : 16.0, shouldImageBeVisible : false, pillBackgroundColor : Color.orange900, pillImage : ""}
      "NEW" -> {text : getString NEW_C, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, cornerRadius : 16.0, shouldImageBeVisible : false, pillBackgroundColor : Color.blue800, pillImage : ""}
      _ -> {text : "", textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, shouldImageBeVisible : false,  cornerRadius : 16.0, pillBackgroundColor : Color.white900, pillImage : ""}

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ BannerCarousel.view push (BannerCarousel.config BannerCarousal)
  void $ EHC.liftFlow $ push (SetBannerItem bannerItem)

bannersCarousal :: forall w. ListItem -> BenefitsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bannersCarousal view state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop 24
  , visibility $ boolToVisibility $ state.props.bannerLength > 0
  ][CarouselHolder.carouselView push $ getCarouselConfig view state]

getCarouselConfig ∷ forall a. ListItem → BenefitsScreenState → CarouselHolder.CarouselHolderConfig BannerCarousel.PropConfig Action
getCarouselConfig view state = {
    view
  , items : BannerCarousel.bannerTransformer $ getRemoteBannerConfigs BannerCarousal
  , orientation : VERTICAL
  , currentPage : state.data.bannerData.currentPage
  , autoScroll : true
  , autoScrollDelay : 5000.0
  , id : "referralBannerCarousel"
  , autoScrollAction : Just UpdateBanner
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerStateChanged
  , onPageScrolled : Nothing
  , currentIndex : state.data.bannerData.currentBanner
  , showScrollIndicator : true
  , layoutHeight : V 100
  , overlayScrollIndicator : true
}


computeReferralListItem :: (Action -> Effect Unit) -> BenefitsScreenState -> Flow GlobalState Unit
computeReferralListItem push state = do
  carouselItem <- preComputeListItem $ referralCardHolder push state
  void $ liftFlow $ push (SetCarouselItem carouselItem)
