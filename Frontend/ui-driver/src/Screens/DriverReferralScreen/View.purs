module Screens.DriverReferralScreen.View where

import Animation as Anim
import Effect (Effect)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), show, (>), (/), (-), (==), (/=), map, (&&), discard, (||))
import PrestoDOM (Accessiblity(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), Visibility(..), PrestoDOM, Screen, accessibility, afterRender, background, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onBackPressed, orientation, padding, weight, width, cornerRadius, textView, text, color, fontStyle, onClick, visibility, relativeLayout, layoutGravity, scrollView, stroke)
import Screens.DriverReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (DriverReferralScreenState, ReferredUserType(..), UserReferralType(..))
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
import Services.API (ReferredDriversReq (..), ReferredDriversResp(..), GetPerformanceRes(..), GetPerformanceReq(..))
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Class (liftEffect)
import Debug (spy)
import Mobility.Prelude
import Engineering.Helpers.BackTrack (liftFlowBT)
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)

screen :: DriverReferralScreenState -> Screen Action DriverReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverReferralScreen"
  , globalEvents: [
              ( \push -> do
                      _ <- launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                        if initialState.data.cityConfig.referralConfig.showDriverReferral then do
                          (ReferredDriversResp referredDriversResp) <- Remote.referredDriversBT (ReferredDriversReq "")
                          liftFlowBT $ push $ ReferredDriversAPIResponseAction referredDriversResp.value
                        else pure unit
                        if initialState.data.cityConfig.referralConfig.showCustomerReferral then do
                          (GetPerformanceRes getPerformanceres) <- Remote.getPerformanceBT (GetPerformanceReq {} )
                          liftFlowBT $ push $ UpdateDriverPerformance (GetPerformanceRes getPerformanceres)
                        else pure unit
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
          , scrollView
            [width MATCH_PARENT
            , weight 1.0
            , height MATCH_PARENT
            , margin $ Margin 8 12 15 15
            -- , visibility if state.props.userReferralType == CUSTOMER_AND_DRIVER_REFERRAL || state.props.userReferralType == CUSTOMER_REFERRAL then VISIBLE else GONE
            , visibility VISIBLE
            ][linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][driverAndCustReferral push state]]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin $ Margin 15 20 15 15
            , orientation VERTICAL
            , visibility GONE
            ][ driverReferralCode push state
             , referredDriversView push state
             , rideLeaderBoard push state
             ]
          , qrScreenView push state
          ]
          , bottomNavBarView push state]
          , if state.props.referralInfo /= NO_POPUP then referralInfoPopUpView push state else linearLayout[][]
          , if state.props.showDriverReferralQRCode then appQRCodeView push state  else linearLayout[][]
  ]


driverAndCustReferral :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
driverAndCustReferral push state =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          ][ textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString REFER_AND_EARN
            , color Color.black800
            , weight 1.0
            ] <> FontStyle.h2 TypoGraphy
           , linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , background Color.blue600
              , cornerRadius 20.0
              , padding (Padding 8 4 8 4)
              , margin $ MarginRight 12
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ KnowMore
              , visibility if state.data.cityConfig.referralConfig.showKnowMore then VISIBLE else GONE
              ][ textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , margin $ MarginLeft 5
                , text $ getString KNOW_MORE
                , weight 1.0
                , gravity CENTER
                , color Color.blue800
                ] <> FontStyle.body3 TypoGraphy
              , imageView
                [ width $ V 8
                , height $ V 8
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_polygon_arrow_right_blue"
                , margin $ Margin 5 5 0 0
                ]
              ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , background Color.yellow800
          , gravity CENTER
          , orientation VERTICAL
          , cornerRadius 16.0
          , margin $ MarginTop 10
          , padding $ Padding 12 12 12 12
          ][textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getString YOUR_REFERRAL_CODE
            , color Color.black900
            , margin $ MarginTop 10
            ] <> FontStyle.subHeading1 TypoGraphy
            , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text state.data.referralCode
            , color Color.black900
            ] <> FontStyle.priceFont TypoGraphy
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , padding $ Padding 15 0 15 0
              , orientation VERTICAL
              , background "#FFFFFF"
              , cornerRadius 12.0
            ](DA.mapWithIndex  (\index item -> referralBar index item push state ((isJust state.props.currentReferralItem) && item.id == (fromMaybe CUSTOMER_REFERRAL state.props.currentReferralItem))) (referralList state) )]
        , referredUsersTextView push state
        , rideLeaderBoard push state
      ]

referralBar :: forall w . Int -> {id :: UserReferralType , avatar_image :: String, text :: String, qr_code_id :: String } -> (Action -> Effect Unit) -> DriverReferralScreenState -> Boolean -> PrestoDOM (Effect Unit) w
referralBar index item push state isqrvisible= 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background "#FFFFFF"
    , padding $ PaddingVertical 10 10
    , orientation VERTICAL
    ][linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][ imageView
            [ height $ V 40
            , width $ V 40
            , imageWithFallback $ fetchImage FF_ASSET item.avatar_image
            ]
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ MarginLeft 12
            , weight 1.0
            ][ linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                ][ textView $
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ getString REFER
                  , color Color.black800
                  ] <> FontStyle.body20 TypoGraphy
                , textView $
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text item.text
                  , color $ if isqrvisible then Color.orange900 else Color.blue800
                  ] <> FontStyle.body20 TypoGraphy
                ]
              , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                ][ textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString EARN_CASH
                    , color Color.black800
                    , margin $ MarginRight 4
                    ] <> FontStyle.body3 TypoGraphy
                , imageView $
                    [ width $ V 20
                    , height $ V 12
                    , margin $ MarginTop 2
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_india_money"
                    ]
                ]
            ]
            , imageView
              [ width $ V 14
              , height $ V 19
              , imageWithFallback $ fetchImage FF_COMMON_ASSET if isqrvisible then "ny_ic_chevron_up" else "ny_ic_chevron_down"
              , gravity RIGHT
              , margin $ MarginTop 10
              , onClick push $ const $ ReferralStageChange item.id
              ]
        ]
    , appQRCode item.qr_code_id push state isqrvisible item.id
    , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , visibility if isqrvisible then VISIBLE else GONE
            , margin $ MarginTop 10
            ][linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , background Color.blue600
              , cornerRadius 20.0
              , padding (Padding 15 4 15 4)
              , margin $ MarginRight 12
              , stroke $ "1," <> Color.grey900
              , onClick push $ const $ ShareOptions item.qr_code_id
              ][imageView
                [ width $ V 16
                , height $ V 16
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_app_share"
                ]
                , textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , margin $ MarginLeft 5
                , text $ getString SHARE
                , weight 1.0
                , gravity CENTER
                , color Color.black900
                ] <> FontStyle.body3 TypoGraphy
              ]
            ]
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
            ] <> FontStyle.priceFont TypoGraphy
         , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            ][ buttonOptions (getString APP_QR_CODE) "ny_ic_app_referral_qr" ShowQRCode push state
             , buttonOptions (getString SHARE) "ny_ic_app_share" (ShareOptions "DriverReferralQR") push state
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
      , referredUsersTextView push state
    ]

referredUsersTextView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
referredUsersTextView push state =
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , cornerRadius 10.0
        , margin $ MarginTop 10
        , padding $ Padding 15 17 15 17
        , gravity CENTER_VERTICAL
        , orientation VERTICAL
        ](map (\item ->
              linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ][textView $
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text item.referred_text
                  , color Color.black800
                  ] <> FontStyle.subHeading2 TypoGraphy
              , imageView
                  [ width $ V 15
                  , height $ V 15
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_grey"
                  , margin $ Margin 5 10 0 0
                  , onClick push $ const $ ReferredInfo item.info_id
                  ]
              , textView $
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , text item.count
                  , color Color.black800
                  , gravity RIGHT
                  ] <> FontStyle.body8 TypoGraphy
          ]) (referredTextList state))
        

referralInfoPopUpView :: forall w . (Action -> Effect Unit) -> DriverReferralScreenState -> PrestoDOM (Effect Unit) w
referralInfoPopUpView push state =
  let _text = if state.props.referralInfo == CUSTOMER then (getString REFERRED_CUSTOMERS) else (getString REFERRED_DRIVERS)
      info_text = if state.props.referralInfo == CUSTOMER then (getString REFERRED_CUSTOMERS_INFO) else (getString REFERRED_DRIVERS_INFO)
  in
    linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER
      , background Color.blackLessTrans
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
              , text _text
              , margin $ MarginVertical 10 8
              , color Color.black800
              ] <> FontStyle.h2 TypoGraphy
            , textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ info_text
              , margin $ MarginHorizontal 14 14
              , gravity CENTER
              , color Color.black700
              ] 
            , textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString GOT_IT
              , margin $ MarginVertical 14 7
              , color Color.blue800
              , onClick push $ const $ BackPressed
              ] <> FontStyle.h2 TypoGraphy
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
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_driver_leaderboard"
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
          -- , appQRCode "DriverReferralQR" push state true
          , PrimaryButton.view (push <<< PrimaryButtonActionController state) (primaryButtonConfig state)
         ]
    ]

appQRCode :: forall w . String -> (Action -> Effect Unit) -> DriverReferralScreenState -> Boolean -> UserReferralType -> PrestoDOM (Effect Unit) w
appQRCode image_id push state isqrvisible itemId = 
  let link = if itemId == DRIVER_REFERRAL then state.data.config.appData.link else state.data.config.customerAppLink
  in 
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , visibility if isqrvisible then VISIBLE else GONE
      ][imageView
            [ width $ V 240
            , height $ V 240
            , id $ getNewIDWithTag image_id
            , afterRender push (const (ReferralQrRendered (getNewIDWithTag image_id) link))
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
          , id $ getNewIDWithTag "DriverReferralScreenLogo"
          , imageWithFallback $ fetchImage FF_ASSET "ic_namma_yatri_logo"
          ]
    ]

referralList :: DriverReferralScreenState -> Array {id :: UserReferralType , avatar_image :: String, text :: String, qr_code_id :: String }
referralList state =
  (if state.data.cityConfig.referralConfig.showCustomerReferral then [{id : CUSTOMER_REFERRAL , avatar_image : "ny_ic_customer_avatar", text : getString CUSTOMER_TEXT, qr_code_id : "CustomerReferralQR"}] else []) <> 
  (if state.data.cityConfig.referralConfig.showDriverReferral then [{id : DRIVER_REFERRAL , avatar_image : "ny_ic_driver_avatar", text : getString DRIVER_TEXT, qr_code_id : "DriverReferralQR"}] else [])

referredTextList :: DriverReferralScreenState -> Array {referred_text :: String, info_id :: ReferredUserType, count :: String}
referredTextList state = 
    (if state.data.cityConfig.referralConfig.showCustomerReferral then
        [{referred_text : getString REFERRED_CUSTOMERS, info_id : CUSTOMER, count : state.data.referredCustomers},
         {referred_text : getString ACTIVATED_CUSTOMERS, info_id : CUSTOMER, count : state.data.activatedCustomers}]    
     else []) <>
    (if state.data.cityConfig.referralConfig.showDriverReferral then
        [{referred_text : getString REFERRED_DRIVERS, info_id : DRIVER, count : state.data.referredDrivers}]
     else [])
-- case state.data.cityConfig.referralConfig.showCustomerReferral, state.data.cityConfig.referralConfig.showDriverReferral of
--                             true, true -> [{referred_text : getString REFERRED_CUSTOMERS, info_id : CUSTOMER, count : state.data.activatedCustomers},
--                                                              {referred_text : getString REFERRED_DRIVERS, info_id : DRIVER, count : state.data.referredDrivers}]
--                             false, true -> [{referred_text : getString REFERRED_DRIVERS, info_id : DRIVER, count : state.data.referredDrivers}]
--                             true, false -> [{referred_text : getString REFERRED_CUSTOMERS, info_id : CUSTOMER, count : state.data.referredCustomers},
--                                                   {referred_text : getString ACTIVATED_CUSTOMERS, info_id : CUSTOMER, count : state.data.activatedCustomers}]
--                             _, _ -> []