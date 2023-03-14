module Screens.ReferralScreen.View where


import Animation (screenAnimationFadeInOut)
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os ,getNewIDWithTag , flowRunner)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (openUrlInApp , startTimerWithTime , toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>) , map , discard , show ,(>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility,stroke , alpha, relativeLayout , scrollView , alignParentRight, alignParentBottom)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Components.PopUpModal as PopUpModal
import Data.Maybe (Maybe(..) ,fromMaybe)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Helpers.Utils (countDown)
import Screens.ReferralScreen.ComponentConfig

screen :: ST.ReferralScreenState -> Screen Action ST.ReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ReferralScreen"
  , globalEvents : []
  , eval
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
        [
          GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state),
          linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
        ] <> if state.props.stage == ST.SuccessScreen then [commonView push "ny_ic_green_tick" (getString  YOUR_REFERRAL_CODE_IS_LINKED) (getString YOU_CAN_NOW_EARN_REWARDS) state] else []
          <> if state.props.stage == ST.ComingSoonScreen then [commonView push "ny_ic_comming_soon_poster" (getString COMING_SOON) (getString COMING_SOON_DESCRIPTION) state] else []
          <> if state.props.stage == ST.ReferralFlow then  [referralEnrolmentFlow push state, continueButtonView push state] else []
          <> if state.props.stage == ST.QRScreen then [qrScreen push state] else [])
        , bottomNavBarView push state
        ]
        , passwordPopUpView push state
        , customerSupportPopUpView state push
    ] <> if state.props.passwordPopUpVisible then [passwordPopUpView push state] else [])

bottomNavBarView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][BottomNavBar.view (push <<< BottomNavBarAction) ( navData 2)]


commonView :: forall w . (Action -> Effect Unit) -> String -> String -> String -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
commonView push img title description state= 
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
                        if state.props.stage == ST.SuccessScreen then do
                          launchAff_ $ flowRunner $ runExceptT $ runBackT $ lift $ lift $ doAff do
                            if (os == "IOS") then liftEffect $ startTimerWithTime (show state.props.seconds) state.props.id "1" push SuccessScreenExpireCountDwon
                              else liftEffect $ countDown state.props.seconds state.props.id push SuccessScreenExpireCountDwon
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
          , textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text title
              , color Color.black900
              , textSize $ FontSize.a_18
              , fontStyle $ FontStyle.bold LanguageStyle
              ]
          , textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text description
              , color Color.black700
              , textSize $ FontSize.a_14
              , fontStyle $ FontStyle.regular LanguageStyle
              , padding (PaddingVertical 10 10)
              ]
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
              title: (getString REFERRAL_CODE)
              , hint: (getString REFERRAL_CODE_HINT)
              , valueId: ""
              , isinValid: false 
              , error: Just (getString INVALID_MOBILE_NUMBER)
              , pattern : Just "[0-9]*,6"
              , text: ""
              , letterSpacing: 0.0
              , id: (getNewIDWithTag "EnterReferralCodeEditText")
              , fontSize : FontSize.a_18
              , type : "number"
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
              , letterSpacing: 0.0
              , id: (getNewIDWithTag "EnterConfirmReferralCoderEditText")
              , fontSize : FontSize.a_18
              , type : "number"
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

qrScreen :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
qrScreen push state =  
  scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][ linearLayout
        [ weight 1.0
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding (Padding 20 0 20 16)
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
                    , imageUrl "ny_namma_yatri"
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
                    , imageUrl "ny_ic_qr_code"
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
              ]
            , textView
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              , text (fromMaybe "" state.data.driverInfo.referralCode)
              , color Color.black900
              , textSize $ FontSize.a_24
              , fontStyle $ FontStyle.bold LanguageStyle
              ]
            , linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , gravity CENTER
              , visibility GONE
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
                    , imageUrl "ic_share"
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
            [  height $ V 80
            , width MATCH_PARENT
            , margin $ MarginTop 16
            , cornerRadius 12.0
            , background if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then Color.greenGrey else Color.black800
            ]
            [  linearLayout
                [ width WRAP_CONTENT
                , height MATCH_PARENT
                , weight 1.0
                , padding $ Padding 20 13 0 0
                , orientation VERTICAL
                ]
                [ textView
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity LEFT
                  , text if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then (getString FIRST_REFERRAL_SUCCESSFUL) else (getString AWAITING_REFERRAL_RIDE)
                  , color Color.white900
                  , textSize FontSize.a_14
                  , fontStyle $ FontStyle.semiBold LanguageStyle
                  ]
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
                ,  margin $ MarginRight 5
                , imageUrl if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then "ny_ic_auto2" else "ny_ic_auto1"
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
              , textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , alignParentRight "true,-1"
                , gravity CENTER
                , text (show state.data.driverPerformance.referrals.totalReferredCustomers)
                , color Color.black800
                , textSize FontSize.a_18
                , fontStyle  $ FontStyle.bold LanguageStyle
                ]
              ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ 
                  textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , gravity LEFT
                  , weight 1.0
                  , text (getString ACTIVATED_CUSTOMERS)
                  , color Color.black800
                  , textSize FontSize.a_14
                  ]
                , textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , alignParentRight "true,-1"
                  , gravity CENTER
                  , text (show state.data.driverPerformance.referrals.totalActivatedCustomers)
                  , color Color.black800
                  , textSize FontSize.a_18
                  , fontStyle $ FontStyle.bold LanguageStyle
                  ]
                ]
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
  ][ textView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity LEFT
    , text (getString FOR_UPDATES_SEE_ALERTS)
    , color Color.white900
    , textSize $ FontSize.a_12
    , fontStyle $ FontStyle.regular LanguageStyle
    ]
    , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.white900
    ][]
  ]


emptyView :: forall w . PrestoDOM (Effect Unit) w
emptyView = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ][]