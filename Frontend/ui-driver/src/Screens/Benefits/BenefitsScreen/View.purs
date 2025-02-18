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
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Screens as ScreenNames
import Language.Strings (getString, getStringEnToHi)
import Language.Types as LT
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState, GlobalState)
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
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
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
import PrestoDOM.List (ListItem, preComputeListItem)
import Engineering.Helpers.Commons as EHC
import Presto.Core.Flow (Flow)
import Data.Function.Uncurried as UC
import JBridge as JB
import DecodeUtil (getAnyFromWindow)
import Data.Function.Uncurried (runFn3)
import Presto.Core.Types.Language.Flow (Flow)
import Presto.Core.Types.API (ErrorResponse(..))
import Data.Function.Uncurried (runFn2)
import Data.Either (Either(..))
import Helpers.API as HelperAPI
import Engineering.Helpers.Utils as EHU
import Common.RemoteConfig.Utils as CRC
import RemoteConfig.Utils as RC
import Helpers.Utils as HU
import Services.API as API
import Data.String as DS
import Data.Array as DA

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
                    if (DA.any (_ == initialState.data.referralCode) ["__failed", "", "(null)"]) then do
                      response <- lift $ lift $ Remote.generateReferralCode (GenerateReferralCodeReq {} )
                      case response of
                        Right (GenerateReferralCodeRes referralCode) -> do
                          lift $ lift $ doAff do liftEffect $ push $ (UpdateReferralCode (GenerateReferralCodeRes referralCode))
                        Left _ -> pure unit
                    else pure unit
                    (LeaderBoardRes leaderBoardResp) <- Remote.leaderBoardBT $ DailyRequest (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                    lift $ lift $ doAff do liftEffect $ push $ UpdateLeaderBoard (LeaderBoardRes leaderBoardResp)
            void $ launchAff $ flowRunner defaultGlobalState do
                moduleResp <- Remote.getAllLmsModules (HU.getLanguageTwoLetters $ Just (getLanguageLocale languageKey))
                case moduleResp of
                  Right modules -> liftFlow $ push $ UpdateModuleList modules
                  Left err -> liftFlow $ push $ UpdateModuleListErrorOccurred
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ computeListItem push
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
  ][ PrestoAnim.animationSet [Anim.fadeIn true] $ 
     linearLayout
     [ width $ MATCH_PARENT
     , height $ MATCH_PARENT
     ][ referralScreenBody push state ]
  , if state.props.showDriverReferralQRCode then appQRCodeView push state else dummyView
  , if state.props.referralInfoPopType /= NO_REFERRAL_POPUP then referralInfoPop push state else dummyView
  ]

referralScreenBody :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralScreenBody push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ MATCH_PARENT
  , orientation VERTICAL
  ][   linearLayout
       [ width $ MATCH_PARENT
       , weight 1.0
       , orientation VERTICAL
       ][ scrollView
          [ height $ MATCH_PARENT
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
  let loadDynamicModule = fromMaybe false $ runFn3 getAnyFromWindow "loadDynamicModule" Nothing Just
      gullakRemoteConfig = CRC.gullakConfig $ getValueToLocalStore DRIVER_LOCATION
  in linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  ]([ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
    , referralStatsView push state
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ Margin 16 0 16 12
      , orientation VERTICAL
      ][ if shouldShowReferral state then driverReferralCode push state else dummyView
      , if loadDynamicModule && gullakRemoteConfig.enabled then savingWithGullak push state gullakRemoteConfig.image else dummyView
      , rideLeaderBoardView push state
      ]
    , learnAndEarnShimmerView push state
  ] <> if not (null state.data.moduleList.completed) || not (null state.data.moduleList.remaining) then [learnAndEarnView push state] else [])

referralStatsView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralStatsView push state =
  let referralBonusVideo = RC.getReferralBonusVideo $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION 
  in 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 12.0
  , orientation VERTICAL
  , padding $ Padding 16 16 16 16
  , margin $ Margin 16 0 16 16
  , stroke $ "1," <> Color.grey900
  , visibility $ boolToVisibility $ state.props.isPayoutEnabled == Just true
  ][ if state.data.payoutAmountPaid > 0 then referralBonusView push state else emptyReferralBonusView push state
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , background Color.green100
     , padding $ Padding 12 6 12 6
     , gravity CENTER_VERTICAL
     , visibility $ boolToVisibility $ referralBonusVideo /= ""
     , margin $ MarginTop 16
     , cornerRadius 6.0
     , onClick (\_ -> if referralBonusVideo /= "" then void $ JB.openUrlInApp referralBonusVideo else pure unit) (const NoAction)
     ][ textView $ 
        [ text $ getString LT.REFERRAL_BONUS_WILL_BE_CREDITED_TO_BANK 
        , color $ Color.black700
        , gravity CENTER_VERTICAL
        , weight 1.0
        , height WRAP_CONTENT
        , singleLine false
        ] <> FontStyle.body3 TypoGraphy
      , imageView
        [ height $ V 22
        , width $ V 22
        , gravity RIGHT
        , margin $ MarginLeft 8
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_youtube"
        ]
     ]
  ]

referralBonusView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralBonusView push state = 
  let lastPayoutAt = fromMaybe "" state.data.lastPayoutAt
  in
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , onClick push $ const $ GoToCustomerReferralTracker false
     ][textView $ 
       [ text $ getString LT.MY_REFERRAL_BONUS
       , color $ Color.black700
       , margin $ MarginRight 36
       ] <> FontStyle.paragraphText TypoGraphy 
     , linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       ][ textView $ 
         [ text $ state.data.config.currency <> show state.data.payoutAmountPaid
         , color Color.limeGreen
         ] <> FontStyle.title2 TypoGraphy
       , textView $ 
         [ text $ getString $ LT.TILL (convertUTCtoISC lastPayoutAt "DD MMM YYYY")
         , color $ Color.black700
         , visibility $ boolToVisibility $ lastPayoutAt /= ""
         , margin $ MarginLeft 4
         ] <> FontStyle.body3 TypoGraphy
       ]
     , linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , visibility $ boolToVisibility $ not $ isNothing state.data.payoutVpa
       ][ textView $ 
          [ textFromHtml $ getString LT.LINKED_UPI_ID <> ": " <> "<b>" <> (fromMaybe "" state.data.payoutVpa) <> "</b>"
          , color $ Color.black700
          ] <> FontStyle.paragraphText TypoGraphy
        ]
     , linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , visibility $ boolToVisibility $ isNothing state.data.payoutVpa
       ][ textView $ 
          [ text $ getString LT.TO_GET_MONEY <> ", "
          , color $ Color.black700
          ] <> FontStyle.paragraphText TypoGraphy
        , linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , cornerRadius 12.0
          , onClick push $ const $ GoToCustomerReferralTracker true
          , padding $ Padding 6 2 6 2
          , background Color.blue800
          ][ textView $ 
             [ text $ getString LT.ADD_UPI_ID
             , color Color.white900
             , padding $ PaddingBottom 1
             ] <> FontStyle.tags TypoGraphy
          ]
        ]
     ]
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity RIGHT
     ][ imageView
       [ height $ V 32
       , width $ V 32
       , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_arrow_right_grey"
       ]
     ]
   ]

emptyReferralBonusView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
emptyReferralBonusView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , onClick push $ const $ GoToCustomerReferralTracker false
  ][ imageView
     [ height $ V 48
     , width $ V 48
     , margin $ MarginRight 16
     , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_coins_stack"
     ]
   , textView $ 
     [ text $ getString $ LT.EARN_FOR_EACH_REFERRAL (state.data.config.currency <> (show $ fromMaybe 0 state.data.payoutRewardAmount))
     , color Color.black900
     , weight 1.0
     , height WRAP_CONTENT
     , margin $ MarginRight 6
     , singleLine false
     ] <> FontStyle.body25 TypoGraphy
   , imageView
     [ height $ V 32
     , width $ V 32
     , imageWithFallback $ HU.fetchImage HU.COMMON_ASSET "ny_ic_arrow_right_grey"
     ] 
  ]

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
    [ tabItem push (state.props.driverReferralType == CUSTOMER) (getString LT.REFER_CUSTOMER) "ny_ic_new_avatar_profile_customer" CUSTOMER bothTabsEnabled $ cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
    ,  tabItem push (state.props.driverReferralType == DRIVER) (getString LT.REFER_DRIVER) "ny_ic_new_avatar_profile" DRIVER bothTabsEnabled $ cityConfig.showDriverReferral || state.data.config.enableDriverReferral
    ]
  where
  cityConfig = HU.getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

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
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET img
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
    cityConfig = HU.getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)

    driverReferral = cityConfig.showDriverReferral || state.data.config.enableDriverReferral

    customerReferral = cityConfig.showCustomerReferral || state.data.config.enableCustomerReferral
  in
    driverReferral || customerReferral

driverReferralCode :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
driverReferralCode push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 20 16 20 16
    , background config.backgroundColor
    , margin $ MarginBottom 16
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
            , onClick push $ const ShowQRCode
            ]
            [ imageView
                [ width $ V 148
                , height $ V 148
                , id (getNewIDWithTag "ReferralQRCode")
                , gravity CENTER
                , padding (Padding 5 5 5 5)
                , afterRender (\action -> do
                                runEffectFn4 HU.generateQR (HU.generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode state.props.driverReferralType) (getNewIDWithTag "ReferralQRCode") 500 0
                              ) (const RenderQRCode)
                ]
            , textView
                $ [ width MATCH_PARENT
                  , gravity CENTER
                  , text $ getString LT.CLICK_TO_EXPAND
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
                , textSize FontSize.a_20
                , margin $ MarginTop 10
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , cornerRadius 24.0
                , background Color.white900
                , orientation HORIZONTAL
                , margin $ MarginTop 12
                , onClick push $ const $ ShareQRLink
                ]
                [ imageView
                  [ height $ V 30
                  , width $ V 30
                  , padding $ PaddingHorizontal 10 5
                  , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_share_grey"
                  ]
                , textView
                  $ [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , color Color.black900
                    , padding $ Padding 0 4 10 0
                    , text $ getString LT.SHARE 
                    ]
                  <> FontStyle.body1 TypoGraphy
                ]
            ]
        ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background config.separatorColor
      , margin $ MarginTop 10
      , visibility $ boolToVisibility $ state.props.isPayoutEnabled == Just false || state.props.driverReferralType == DRIVER
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      , visibility $ boolToVisibility $ state.props.isPayoutEnabled == Just false || state.props.driverReferralType == DRIVER
      ][ referralCountView false (getString LT.REFERRED) (show state.data.totalReferredCustomers) (state.props.driverReferralType == CUSTOMER) push REFERRED_CUSTOMERS_POPUP
       , referralCountView true config.infoText (show activatedCount) true push config.popupType
       ]
    ]
  where
  activatedCount = if state.props.driverReferralType == DRIVER then state.data.totalReferredDrivers else state.data.totalActivatedCustomers

  config = if state.props.driverReferralType == DRIVER then driverReferralConfig else customerReferralConfig

  driverReferralConfig =
    let appConfigs = getAppConfig appConfig
    in  {backgroundColor: Color.yellow900
        , qr_img: "ny_driver_app_qr_code"
        , infoText: getString LT.REFERRED_DRIVERS
        , separatorColor: Color.white300
        , popupType: REFERRED_DRIVERS_POPUP
        , referralText: LT.DRIVER_REFERRAL_CODE
        , referralDomain : appConfigs.appData.website
        }

  customerReferralConfig =
    let appConfigs = getAppConfig appConfig
    in  { backgroundColor: Color.frenchSkyBlue800
        , qr_img: "ny_customer_app_qr_code"
        , infoText: getString LT.ACTIVATED
        , separatorColor: Color.frenchSkyBlue400
        , popupType: ACTIVATED_CUSTOMERS_POPUP
        , referralText: LT.CUSTOMER_REFERRAL_CODE
        , referralDomain : appConfigs.appData.website
        }

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
            $ [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_star_black"
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
            $ [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_info_black"
              , height $ V 14
              , width $ V 14
              , margin $ Margin 4 2 0 0
              , onClick push $ const $ ShowReferedInfo popupType
              , padding $ PaddingBottom 2
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

appQRCodeView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
appQRCodeView push state =
  let appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  in linearLayout
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
              , text $ getString $ LT.DOWNLOAD_NAMMA_YATRI appName
              , margin $ MarginVertical 10 7
              , color Color.black800
              ]
            <> FontStyle.h2 TypoGraphy
        , imageView
            [ width MATCH_PARENT
            , height $ V 280
            , gravity CENTER
            , id (getNewIDWithTag "ExpandedReferralQRCode")
            , padding (Padding 5 5 5 5)
            , afterRender (\action -> do
                            runEffectFn4 HU.generateQR (HU.generateReferralLink (getValueToLocalStore DRIVER_LOCATION) "qrcode" "referral" "coins" state.data.referralCode state.props.driverReferralType) (getNewIDWithTag "ExpandedReferralQRCode") 280 0
                          ) (const RenderQRCode)
            ]
        , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
        ]
    ]
  where
  qr_img = if state.props.driverReferralType == DRIVER then "ny_driver_app_qr_code" else "ny_customer_app_qr_code"

referralInfoPop :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
referralInfoPop push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    , onClick push $ const $ ShowReferedInfo NO_REFERRAL_POPUP
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
              , text $ getString LT.GOT_IT
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
    REFERRED_DRIVERS_POPUP -> { heading: getString LT.REFERRED_DRIVERS, subtext: getString $ LT.REFERRED_DRIVERS_INFO "REFERRED_DRIVERS_INFO" }
    REFERRED_CUSTOMERS_POPUP -> { heading: getString LT.REFERRED_CUSTOMERS, subtext: getString $ LT.REFERRED_CUSTOMERS_INFO "REFERRED_CUSTOMERS_INFO" }
    ACTIVATED_CUSTOMERS_POPUP -> { heading: getString LT.ACTIVATED_CUSTOMERS, subtext: getString LT.ACTIVATED_CUSTOMERS_INFO }
    _ -> { heading: "", subtext: "" }

savingWithGullak :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> String -> PrestoDOM (Effect Unit) w
savingWithGullak push state image =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginBottom 10
    ]
    [ textView
        $ [ text "Invest in gold"
          , color Color.black800
          , margin $ MarginBottom 6
          ]
        <> FontStyle.h2 TypoGraphy
    , relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        , cornerRadius 12.0
        , clickable state.props.glBannerClickable
        ][  shimmerFrameLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][ linearLayout
              [ width $ MATCH_PARENT
              , background Color.greyDark
              , height $ V 140
              , margin $ MarginVertical 20 20
              , cornerRadius $ 16.0
              ][]
            ]
          , imageView
            [ width MATCH_PARENT
            , height $ V 180
            , onClick (\action -> do
                  void $ push action
                  void $ launchAff $ flowRunner defaultGlobalState $ checkTokenAndInitSDK push GullakSDKResponse
                  pure unit)
                (const GullakBannerClick)
            , gravity CENTER
            , imageUrl image
            ]
        ]
    ]

rideLeaderBoardView :: forall w. (Action -> Effect Unit) -> BenefitsScreenState -> PrestoDOM (Effect Unit) w
rideLeaderBoardView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ text $ getStringEnToHi LT.RIDE_LEADERBOARD
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
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_leaderboard"
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
                $ [ text $ if driverNotInLBdOrLBNotReady then getStringEnToHi LT.YOUR_DAILY_RANK else getStringEnToHi LT.ACCEPT_RIDE_TO_ENTER_LEADERBOARD
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
            , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_arrow_right_circle_green"
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
     [ text $ getString LT.LEARN_AND_EARN
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
                              MODULE_NOT_YET_STARTED -> {moduleStatus : "NEW", moduleMargin : MarginRight 5, noOfVideosToDisplay : show moduleInfo.noOfVideos <> " " <> getString LT.VIDEOS}
                              MODULE_ONGOING -> {moduleStatus : "PENDING", moduleMargin : if zeroVideosLeft then MarginRight 0 else MarginRight 5, 
                                                noOfVideosToDisplay : if zeroVideosLeft then "" else show (moduleInfo.noOfVideos - moduleInfo.noOfVideosCompleted) <> " " <> getString LT.VIDEOS}
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
     , imageWithFallback $ HU.fetchImage HU.FF_ASSET pillProperty.pillImage
     , margin $ MarginRight 3]
  ,  textView $
     [ text $ pillProperty.text
     , color $ pillProperty.textColor
     ] <> pillProperty.fontStyle
  ]
  where
    getPropertyAccordingToStatus :: {text :: String, textColor :: String, fontStyle :: forall properties. (Array (Prop properties)), cornerRadius :: Number, shouldImageBeVisible :: Boolean, pillBackgroundColor :: String, pillImage :: String}
    getPropertyAccordingToStatus = case status of
      "COMPLETED" -> {text : getString LT.COMPLETED_STR, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, cornerRadius : 16.0, shouldImageBeVisible : true, pillBackgroundColor : Color.green900, pillImage : "ny_ic_white_tick"} 
      "PENDING" -> {text : getString LT.PENDING_STR_C, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle,  cornerRadius : 16.0, shouldImageBeVisible : false, pillBackgroundColor : Color.orange900, pillImage : ""}
      "NEW" -> {text : getString LT.NEW_C, textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, cornerRadius : 16.0, shouldImageBeVisible : false, pillBackgroundColor : Color.blue800, pillImage : ""}
      _ -> {text : "", textColor : Color.white900, fontStyle : FontStyle.body19 LanguageStyle, shouldImageBeVisible : false,  cornerRadius : 16.0, pillBackgroundColor : Color.white900, pillImage : ""}

computeListItem :: (Action -> Effect Unit) -> Flow GlobalState Unit
computeListItem push = do
  bannerItem <- preComputeListItem $ BannerCarousel.view push (BannerCarousel.config BannerCarousal)
  void $ EHC.liftFlow $ push (SetBannerItem bannerItem)

checkTokenAndInitSDK :: forall action. (action -> Effect Unit) ->  (String -> action) -> Flow GlobalState Unit
checkTokenAndInitSDK push action = do
  let tokenWithExp = DS.split (DS.Pattern "<$>") (getValueToLocalStore GULLAK_TOKEN)
      cachedToken = fromMaybe "" (tokenWithExp DA.!! 0)
      isTokenValid = (runFn2 JB.differenceBetweenTwoUTC (fromMaybe "" (tokenWithExp DA.!! 1)) (HU.getCurrentUTC "")) > 0
  if isTokenValid && not (DS.null cachedToken) then do
    void $ pure $ setValueToLocalStore DONT_CALL_REFRESH "true"
    void $ pure $ UC.runFn4 JB.emitJOSEventWithCb "gl_sdk" (JB.josEventInnerPayload {param1 = cachedToken, param2 = "false"}) push action
    pure unit
  else do
    void $ EHU.loaderText (getString LT.LOADING) (getString LT.PLEASE_WAIT_WHILE_IN_PROGRESS)
    EHU.toggleLoader true
    response <- HelperAPI.callApi $ API.GetSdkTokenReq "0" API.Gullak
    case response of
      Left _ -> do
        void $ pure $ JB.toast $ getString LT.ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
        pure unit
      Right (API.GetSdkTokenResp resp) -> do
        void $ pure $ setValueToLocalStore GULLAK_TOKEN $ resp.token <> "<$>" <> fromMaybe "" resp.expiry
        void $ pure $ setValueToLocalStore DONT_CALL_REFRESH "true"
        void $ pure $ UC.runFn4 JB.emitJOSEventWithCb "gl_sdk" (JB.josEventInnerPayload {param1 = resp.token, param2 = "false"}) push action
        pure unit
  pure unit