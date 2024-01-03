module Screens.DriverReferralScreen.View where

import Animation as Anim
import Effect (Effect)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), show, (>), (/), (-))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, accessibility, afterRender, background, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, orientation, padding, weight, width, cornerRadius, textView, text, color, fontStyle, onClick, visibility, relativeLayout, layoutGravity)
import Screens.DriverReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (DriverReferralScreenState)
import Screens.DriverReferralScreen.ComponentConfig
import Styles.Colors as Color
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
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
import Services.API (ReferredDriversReq (..), ReferredDriversResp(..))
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Debug (spy)
import Mobility.Prelude
import Engineering.Helpers.BackTrack (liftFlowBT)

screen :: DriverReferralScreenState -> Screen Action DriverReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverReferralScreen"
  , globalEvents: [
              ( \push -> do
                      _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                        (ReferredDriversResp referredDriversResp) <- Remote.referredDriversBT (ReferredDriversReq "")
                        liftFlowBT $ push $ ReferredDriversAPIResponseAction referredDriversResp.value
                      pure $ pure unit
              )
  ]
  , eval:
      ( \action state -> do
          let _ = spy "DriverReferralScreen ----- state" state
          let _ = spy "DriverReferralScreen --------action" action
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
  ][
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][  linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        , height WRAP_CONTENT
        ]
        [ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ][ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)]
          , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ Margin 15 20 15 15
            , orientation VERTICAL
            ][ driverReferralCode push state
             , referredDriversView push state
             , rideLeaderBoard push state
             ]
          , qrScreenView push state
          ]
          , bottomNavBarView push state]
          , if state.props.showDriverReferralQRCode then appQRCodeView push state  else linearLayout[][]
  ]


driverReferralCode :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
driverReferralCode push state =
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ Padding 18 15 8 15
    , background state.data.config.primaryTextColor
    , cornerRadius 16.0
    , gravity CENTER
    ][   imageView
            [ height $ V 28
            , width $ V 70
            , imageWithFallback $ fetchImage FF_ASSET "ic_namma_yatri_logo"
            ]
         , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString DRIVER_REFERRAL_CODE
            , color Color.black900
            , margin $ MarginVertical 10 0
            ] <> FontStyle.subHeading1 TypoGraphy
         , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text state.data.referralCode
            , color Color.black900
            , fontStyle $ FontStyle.bold LanguageStyle
            ] <> FontStyle.priceFont TypoGraphy
         , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            ][ buttonOptions (getString APP_QR_CODE) "ny_ic_app_referral_qr" ShowQRCode push state
             , buttonOptions (getString SHARE_OPTIONS) "ny_ic_app_share" ShareOptions push state
             ]
     ]


buttonOptions :: forall w . String -> String -> Action -> (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
buttonOptions _text image action push state =
  linearLayout
  [ width $ V $ (screenWidth unit/2) - 40
  , height WRAP_CONTENT
  , background state.data.config.popupBackground
  , cornerRadius 20.0
  , padding (Padding 10 8 10 8)
  , margin $ MarginRight 12
  ][imageView
    [ width $ V 20
    , height $ V 20
    , imageWithFallback $ fetchImage FF_COMMON_ASSET image
    ]
    , textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ MarginLeft 5
    , text _text
    , weight 1.0
    , gravity CENTER
    , color Color.black900
    , onClick push $ const $ action
    ] <> FontStyle.paragraphText TypoGraphy
  ]

referredDriversView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
referredDriversView push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 10
    , orientation VERTICAL
    ][textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text $ getString $ START_TAKING_RIDES_AND_REFER "START_TAKING_RIDES_AND_REFER"
        , background Color.blue600
        , color Color.black800
        , cornerRadius 10.0
        , padding (Padding 15 8 15 8)
        , visibility $ boolToVisibility state.props.showNewDriverReferralText
        ] <> FontStyle.subHeading2 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , cornerRadius 10.0
        , margin $ MarginTop 10
        , padding $ Padding 15 17 15 17
        ][textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString REFERRED_DRIVERS
            , color Color.black800
            ] <> FontStyle.subHeading2 TypoGraphy
         , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , text state.data.referredDrivers
            , color Color.black800
            , gravity RIGHT
            ] <> FontStyle.body8 TypoGraphy
         ]
     
    ]

rideLeaderBoard :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
rideLeaderBoard push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 15
    ][ textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text $ getString RIDE_LEADERBOARD
        , color Color.black800
        ] <> FontStyle.body7 TypoGraphy

     , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background state.data.config.primaryBackground
        , cornerRadius 10.0
        , padding $ Padding 15 15 15 15
        , margin $ MarginTop 10
        ][ imageView
            [ width $ V 97
            , height $ V 97
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_driver_leaderboard"
            ]
          , linearLayout
            [weight 1.0][]
          , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , margin $ MarginRight 10
                , gravity CENTER
                ][ textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString YOUR_RANK
                    , color state.data.config.popupBackground
                    ] <> FontStyle.body3 TypoGraphy
                , textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString NOT_AVAILABLE_YET
                    , color state.data.config.popupBackground
                    , margin $ MarginTop 5
                    ] <> FontStyle.body5 TypoGraphy
                , textView
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString LEARN_MORE
                    , background state.data.config.popupBackground
                    , color state.data.config.primaryBackground
                    , cornerRadius 10.0
                    , padding $ Padding 15 8 15 8
                    , onClick push $ const $ LearnMore
                    , margin $ MarginTop 5
                    ]
                 ]
              ]
     ]

bottomNavBarView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.REFERRAL_SCREEN state.data.config.bottomNavConfig)]

appQRCodeView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
appQRCodeView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity CENTER
    , background Color.blackLessTrans
    , onClick push $ const $ BackPressed
    ][
      linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , cornerRadius 16.0
        , background state.data.config.popupBackground
        , margin $ MarginHorizontal 10 10
        , padding $ Padding 12 12 12 12
        ][ textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , text $ getString $ DOWNLOAD_NAMMA_YATRI "DOWNLOAD_NAMMA_YATRI"
            , margin $ MarginVertical 10 7
            , color Color.black800
            ] <> FontStyle.h2 TypoGraphy
          , imageView
            [ width $ V 280
            , height $ V 280
            , gravity CENTER
            , imageWithFallback $ fetchImage FF_ASSET "ny_driver_app_qr_code"
            ]
          , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
         ]
    ]

qrScreenView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
qrScreenView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ][ imageView 
            [ height $ V 0
            , width $ V 0
            , id $ getNewIDWithTag "DriverReferralQRScreen"
            , imageWithFallback $ fetchImage FF_ASSET "ny_driver_app_qr_code"
            ]
          , imageView 
            [ height $ V 0
            , width $ V 0
            , id $ getNewIDWithTag "DriverReferralScreenLogo"
            , imageWithFallback $ fetchImage FF_ASSET "ic_namma_yatri_logo"
            ]
    ]