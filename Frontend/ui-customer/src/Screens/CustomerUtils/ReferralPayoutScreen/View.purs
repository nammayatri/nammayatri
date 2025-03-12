{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralPayoutScreen.View where

import Common.Types.App
import Screens.ReferralPayoutScreen.ComponentConfig
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.ComplaintsModel as ComplaintsModel
import Components.GenericHeader as GenericHeader
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getCityConfig)
import JBridge as JB
import Language.Strings (getString, getVarString)
import Components.MenuButton as MenuButton
import Components.PrimaryButton as PrimaryButton
import RemoteConfig as RemoteConfig
import Language.Types (STR(..))
import Helpers.Referral (generateReferralLink)
import Prelude
import PrestoDOM
import Screens.ReferralPayoutScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.ReferralPayoutScreen.ComponentConfig (genericHeaderConfig)
import Screens.Types as ST
import Screens.ReferralPayoutScreen.ScreenData as ST
import Screens.ReferralPayoutScreen.ScreenData
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Colors
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)
import Data.String as DS
import Data.Array as DA
import Language.Types as LT
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import PrestoDOM.Elements.Elements
import PrestoDOM.Animation as PrestoAnim
import Debug
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Effect.Aff
import Services.Backend as Remote
import Engineering.Helpers.Commons
import Types.App
import Services.API
import Data.Either
import Data.Int (fromNumber)

screen :: ST.ReferralPayoutScreenState -> Screen Action ST.ReferralPayoutScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "ReferralPayoutScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.isEarnings then do
              fiber <-
                launchAff $ flowRunner defaultGlobalState
                  $ do
                      eiResp <- Remote.getPayoutHistory ""
                      case eiResp of
                        Right resp -> liftFlow $ push $ HandlePayoutHistory resp
                        Left err -> liftFlow $ push $ HandlePayoutHistory (PayoutHistoryResp { history: [] })
              pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber
            else
              pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "ReferralPayoutScreen action " action
          let
            _ = spy "ReferralPayoutScreen state " state
          eval action state
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , afterRender push (const AfterRender)
        ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Colors.white900
          ]
          [ GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
          , linearLayout
              [ weight 1.0
              , width MATCH_PARENT
              ]
              [ Keyed.scrollView
                  [ width MATCH_PARENT
                  , height MATCH_PARENT
                  ]
                  [ if state.props.isEarnings then Tuple "earningsView" $ earningsView push state else Tuple "shareAndEarnView" $ shareAndEarnView push state ]
              ]
          ]
      , referralFaqsView push state
      ]
    <> (if state.props.showShareAppQr then [ referralQrView push state ] else [])
    <> (if state.props.showUPIPopUp then [ upiIdVerificationPopUp push state ] else [])
    <> (if state.props.showUpiSuccess then [ upiAddedSuccessfully push state ] else [])

shareAndEarnView push state =
  let
    totalEarning = state.data.referralEarnings + state.data.referredByEarnings
  in
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ] (if totalEarning > 0.0 then [collectEarningView push state] else []
    
    <> [ coverView push state
    , referralDescriptionView push state
    ])

earningsView push state =
  PrestoAnim.animationSet
    [ Anim.translateInXForwardAnim true, Anim.fadeIn true
    ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding $ Padding 16 16 16 16
        , background Colors.white900
        ]
        [ linearLayout
            [ background Colors.blue600
            , cornerRadius 24.0
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 16 16 16 16
            ]
            [ linearLayout
                [ weight 1.0
                , height WRAP_CONTENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text state.data.vpa
                      , color Colors.black900
                      , ellipsize true
                      , singleLine true
                      ]
                    <> FontStyle.title2Italic TypoGraphy
                , textView
                    $ [ text $ getString YOUR_EARNINGS_WILL_BE_CREDITED_TO_THIS_ACCOUNT
                      , color Colors.black900
                      ]
                    <> FontStyle.body3 TypoGraphy
                ]
            , linearLayout
                [ width WRAP_CONTENT
                , gravity CENTER
                , height MATCH_PARENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity CENTER
                    , padding $ Padding 12 8 12 8
                    , stroke $ "1," <> Colors.grey900
                    , cornerRadius 24.0
                    , onClick push $ const AddUPI
                    , rippleColor Colors.rippleShade
                    , background Colors.white900
                    ]
                    [ textView
                        $ [ text $ getString EDIT
                          , height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , color Colors.black800
                          ]
                        <> FontStyle.body1 TypoGraphy
                    ]
                ]
            ]
        , transactionView push state
        ]

transactionView push state =
  let
    len = DA.length [ 1, 2 ]
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Colors.white900
      , margin $ MarginTop 32
      ]
      [ textView
          $ [ text $ getString TRANSACTION_HISTORY
            , color Colors.black800
            ]
          <> FontStyle.h2 TypoGraphy
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , margin $ MarginTop 12
          , cornerRadius 32.0
          ]
          [ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , background "#F1F2F7"
              , padding $ Padding 16 12 16 12
              , cornerRadii $ Corners 12.0 true true false false
              ]
              [ textView
                  $ [ text $ getString TOTAL_EARNED
                    , color Colors.black700
                    ]
                  <> FontStyle.paragraphText TypoGraphy
              , linearLayout [ weight 1.0 ] []
              , textView
                  $ [ text $ "₹" <> show (state.data.referralEarnings + state.data.referredByEarnings)
                    , color Colors.black700
                    ]
                  <> FontStyle.h2 TypoGraphy
              ]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ]
              ( DA.mapWithIndex
                  ( \idx item ->
                      linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        $ [ linearLayout
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , padding $ Padding 16 12 16 12
                              ]
                              [ linearLayout
                                  [ weight 1.0
                                  , height WRAP_CONTENT
                                  , orientation VERTICAL
                                  ]
                                  [ textView
                                      $ [ text $ getString TOOK_CAB_RIDE_USING_REFERRAL_CODE
                                        , color Colors.black900
                                        ]
                                      <> FontStyle.body1 TypoGraphy
                                  , textView
                                      $ [ text $ "31_AUG_2022__31_AUG_2022"
                                        , color Colors.black600
                                        ]
                                      <> FontStyle.body1 TypoGraphy
                                  ]
                              , linearLayout
                                  [ width WRAP_CONTENT
                                  , height WRAP_CONTENT
                                  , orientation VERTICAL
                                  , gravity RIGHT
                                  ]
                                  [ textView
                                      $ [ text $ "50"
                                        , color Colors.black900
                                        ]
                                      <> FontStyle.subHeading2 TypoGraphy
                                  , linearLayout
                                      [ width WRAP_CONTENT
                                      , height WRAP_CONTENT
                                      , padding $ Padding 10 4 10 4
                                      , background Colors.green900
                                      , cornerRadius 15.0
                                      ]
                                      [ textView
                                          $ [ text $ getString SUCCESS
                                            , color Colors.white900
                                            ]
                                          <> FontStyle.captions TypoGraphy
                                      ]
                                  ]
                              ]
                          ]
                        <> if len == (idx + 1) then [] else [ linearLayout [ height $ V 1, background Colors.grey900, width MATCH_PARENT ] [] ]
                  )
                  [ 1, 2 ]
              )
          ]
      ]

coverView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
coverView push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background "#B0D4EE"
    , gravity CENTER
    , orientation VERTICAL
    , padding $ Padding 20 20 20 20
    ]
    [ imageView
        [ imageWithFallback "ny_ic_referral_r2r_cover_1,"
        , height $ V 220
        , width MATCH_PARENT
        ]
    , textView
        $ [ text $ getString INVITE_AND_EARN_
          , color Colors.black900
          ]
        <> FontStyle.title2Italic TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , padding $ Padding 16 16 16 16
        , background Colors.white900
        , cornerRadius 24.0
        , margin $ MarginTop 20
        , gravity CENTER
        ]
        [ inviteCodeView push state
        , shareOptionsView push state
        ]
    ]

inviteCodeView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
inviteCodeView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , rippleColor Colors.rippleShade
        , cornerRadius 10.0
        , onClick push $ const CopyToClipboard
        ]
        [ textView
            $ [ text state.data.referralCode
              , color "#14171F"
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.h1 TypoGraphy
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy_blue"
            , height $ V 24
            , width $ V 24
            ]
        ]
    , textView
        $ [ text $ getString YOUR_INVITE_CODE
          , color "#14171F"
          ]
        <> FontStyle.body1 TypoGraphy
    ]

shareOptionsView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
shareOptionsView push state =
  linearLayout
    [ width WRAP_CONTENT
    , weight 1.0
    , height WRAP_CONTENT
    , gravity RIGHT
    ]
    $ map
        ( \item ->
            linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , padding $ Padding 16 8 16 8
              , background Colors.blue900
              , cornerRadius 34.0
              , margin $ MarginLeft 8
              , rippleColor Colors.rippleShade
              , onClick push $ const item.action
              ]
              [ item.view
              ]
        )
        ( [ { view:
                ( textView
                    $ [ text $ getString SHARE
                      , color Colors.white900
                      , padding $ PaddingBottom 3
                      ]
                    <> FontStyle.h3 TypoGraphy
                )
            , action: ShareLink
            }
          , { view:
                imageView
                  [ imageWithFallback "ny_ic_qr_thumbnail_white,"
                  , height $ V 28
                  , width $ V 28
                  ]
            , action: ShareQR
            }
          ]
        )

referralDescriptionView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralDescriptionView push state =
  linearLayout
    [ background Colors.white900
    , padding $ Padding 16 16 16 16
    , width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Colors.blue600
        , padding $ Padding 20 20 20 20
        , cornerRadius 24.0
        , gravity CENTER
        , orientation VERTICAL
        ]
        $ (descView state)
        <> [ haveQuestionView push state
          ]
    ]

descView :: forall w. ST.ReferralPayoutScreenState -> Array (PrestoDOM (Effect Unit) w)
descView state =
  let referralPayoutConfig = RemoteConfig.getReferralPayoutConfig (getValueToLocalStore CUSTOMER_LOCATION)
      youGet = fromMaybe 0 $ fromNumber $ fromMaybe 0.0 referralPayoutConfig.youGet
      theyGet = fromMaybe 0 $ fromNumber $ fromMaybe 0.0 referralPayoutConfig.theyGet
  in map
    ( \item ->
        linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginVertical 10 10
          ]
          [ textView
              $ [ 
                  text $ item.title
                , color Colors.black900
                ]
              <> FontStyle.body8 TypoGraphy
          , textView
              $ [ 
                  text $ item.desc
                , color Colors.black900
                ]
              <> FontStyle.body1 TypoGraphy
          ]
    )
    ( [ { title: getString YOU_GET
        , desc: (if youGet > 0 then "₹" <> show youGet else (getString AMOUNT)) <>  " " <> (getString CREDITED_TO_THE_REFEREES_ACCOUNT_WHEN_THE_REFEREE__USE_YOUR_REFERRAL_CODE_FOR_APP_INSTALLATION_AND_THEN_TAKES_A_VALID_RIDE)
        }
      , { title: getString THEY_GET
        , desc: (if theyGet > 0 then "₹" <> show theyGet else (getString AMOUNT)) <> (getString CREDITED_TO_YOUR_ACCOUNT_WHEN_YOUR_REFEREE_USES_YOUR_REFERRAL_CODE_FOR_APP_INSTALLATION_AND_USES_THE_APP_FOR_TAKING_A_VALID_RIDE)
        }
      ]
    )

haveQuestionView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
haveQuestionView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    ]
    [ textView
        $ [ text $ getString HAVE_QUESTIONS
          , color Colors.black900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    , textView
        $ [ text $ getString VIEW_THE_FAQS
          , color Colors.blue800
          , onClick push $ const ShowReferralFAQ
          , rippleColor Colors.rippleShade
          , cornerRadius 24.0
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

referralFaqsView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralFaqsView push state =
  PrestoAnim.animationSet
    [ PrestoAnim.Animation
        [ PrestoAnim.duration 150
        , PrestoAnim.fromAlpha 0.0
        , PrestoAnim.toAlpha 1.0
        ]
        state.props.showReferralFaq
    ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background $ if state.props.showReferralFaq then Colors.blackLessTrans else Colors.transparent
        , gravity BOTTOM
        , onClick push $ const CloseReferralFAQ
        , clickable state.props.showReferralFaq
        ]
        [ coordinatorLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            ]
            [ bottomSheetLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , accessibility DISABLE
                , enableShift false
                , hideable true
                , sheetState $ if state.props.showReferralFaq then EXPANDED else HIDDEN
                , orientation VERTICAL
                , onStateChanged push SheetStateChanged
                ]
                [ linearLayout
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , gravity BOTTOM
                    ]
                    [ linearLayout
                        [ height WRAP_CONTENT
                        , orientation VERTICAL
                        , width MATCH_PARENT
                        , background Colors.white900
                        , padding $ Padding 16 20 16 20
                        , cornerRadii $ Corners 24.0 true true false false
                        , gravity CENTER
                        ]
                        [ textView
                            $ [ text $ getString REFERRAL_FAQS
                              , color Colors.black900
                              ]
                            <> FontStyle.h1 TypoGraphy
                        , linearLayout
                            [ height WRAP_CONTENT
                            , width WRAP_CONTENT
                            , orientation VERTICAL
                            ]
                            ( DA.mapWithIndex
                                ( \idx item ->
                                    linearLayout
                                      [ width MATCH_PARENT
                                      , height WRAP_CONTENT
                                      , margin $ MarginVertical 5 5
                                      ]
                                      [ textView
                                          $ [ text $ (show (idx + 1)) <> ". "
                                            , color Colors.black800
                                            ]
                                          <> FontStyle.body2 TypoGraphy
                                      , textView
                                          $ [ 
                                             text item
                                            , color Colors.black800
                                            ]
                                          <> FontStyle.body2 TypoGraphy
                                      ]
                                )
                                ( [ getString TO_GET_THE_REFERRAL_REWARD_NEW_USER_SHOULD_INSTALL_THE_APP_AND_ENTER_THE_REFERRAL_CODE_BEFORE_TAKING_THEIR_FIRST_RIDE
                                  , getString THE_REFERRAL_REWARD_WILL_BE_GIVEN_ONLY_FOR_THE_FIRST_RIDE_POST_CHECKING_THE_RIDE_VALIDITY
                                  , getString THE_REFERRAL_REWARD_PROGRAM_IS_A_TIME_BOUND_PROGRAM_AND_CAN_BE_STOPPED_AT_ANY_TIME_WITHOUT_PRIOR_INTIMATION
                                  , getString NAMMA_YATRI_RESERVES_THE_RIGHT_TO_DETERMINE_THE_ELIGIBILITY_OF_A_CUSTOMER_TO_BE_VALID_FOR_REFERRAL_REWARD_OR_NOT
                                  ]
                                )
                            )
                        , linearLayout
                            [ background Colors.white900
                            , stroke $ "1," <> Colors.black500
                            , width MATCH_PARENT
                            , height WRAP_CONTENT
                            , padding $ PaddingVertical 14 14
                            , gravity CENTER
                            , cornerRadius 10.0
                            , rippleColor Colors.rippleShade
                            , onClick push $ const CloseReferralFAQ
                            , margin $ MarginTop 20
                            ]
                            [ textView
                                $ [ text $ getString GOT_IT
                                  , color Colors.black700
                                  , padding $ PaddingBottom 3
                                  ]
                                <> FontStyle.subHeading1 TypoGraphy
                            ]
                        ]
                    ]
                ]
            ]
        ]

upiIdVerificationPopUp :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
upiIdVerificationPopUp push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Colors.blackLessTrans
    , gravity CENTER
    , padding $ Padding 24 24 24 24
    , onClick push $ const CloseUPI
    ]
    [ scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 16 0 16 16
            , background Colors.white900
            , gravity CENTER
            , cornerRadius 10.0
            ]
            [ imageView
                [ imageWithFallback "ny_ic_referral_r2r_cover_2,"
                , height $ V $ (EHC.screenWidth unit) - 150
                , width $ V $ (EHC.screenWidth unit) - 80
                ]
            , textView
                $ [ text $ getString COLLECT_REFERRAL_EARNINGS
                  , color Colors.black800
                  ]
                <> FontStyle.h2 TypoGraphy
            , textView
                $ [ text $ getString VERIFY_YOUR_UPI_ID_TO_RECEIVE_YOUR_REFERRAL_EARNINGS
                  , margin $ MarginTop 8
                  , color Colors.black700
                  ]
                <> FontStyle.body1 TypoGraphy
            , referralView push state
            , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
            ]
        ]
    ]

referralView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralView push state =
  let
    isVerifyVpaEnable = DS.length state.data.vpa >= 6
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 16
      ]
      [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , cornerRadius 8.0
          , gravity CENTER_VERTICAL
          , stroke $ "1," <> Colors.borderColorLight
          ]
          [ editText
              $ [ height MATCH_PARENT
                , width WRAP_CONTENT
                , weight 1.0
                , padding $ Padding 20 15 20 15
                , color Colors.black800
                , onChange push VpaTextChanged
                , gravity LEFT
                , cornerRadius 8.0
                , hint $ "Enter UPI ID"
                , hintColor Colors.black600
                , inputType TypeText
                , id $ EHC.getNewIDWithTag "VpaEditText"
                , accessibility ENABLE
                , accessibilityHint
                    $ case state.data.verificationStatus of
                        ST.UpiVerified -> "UPI ID Verified!"
                        ST.UpiNotVerified -> "Enter UPI ID"
                        ST.UpiFailed -> "UPI ID verification failed"
                        ST.UpiVerifying -> "Verifying UPI ID"
                ]
              <> FontStyle.subHeading1 LanguageStyle
              <> if state.data.verificationStatus == UpiVerified then [ text state.data.vpa, setCursorAtEnd true ] else []
          , linearLayout
              [ height MATCH_PARENT
              , width WRAP_CONTENT
              , cornerRadius 20.0
              , gravity CENTER_VERTICAL
              , margin $ Margin 10 10 10 10
              , padding $ Padding 12 4 12 4
              , onClick push $ const VerifyVPA
              , clickable $ isVerifyVpaEnable && state.data.verificationStatus == UpiNotVerified
              , alpha if isVerifyVpaEnable then 1.0 else 0.4
              , background case state.data.verificationStatus of
                  ST.UpiVerified -> Colors.green900
                  ST.UpiNotVerified -> Colors.blue800
                  ST.UpiFailed -> Colors.red900
                  ST.UpiVerifying -> Colors.black700
              ]
              [ imageView
                  $ [ width
                        $ V
                            ( case state.data.verificationStatus of
                                ST.UpiNotVerified -> 0
                                ST.UpiVerifying -> 0
                                _ -> 18
                            )
                    , height
                        $ V
                            ( case state.data.verificationStatus of
                                ST.UpiNotVerified -> 0
                                ST.UpiVerifying -> 0
                                _ -> 18
                            )
                    , margin $ Margin 0 2 2 2
                    , imageWithFallback
                        $ case state.data.verificationStatus of
                            ST.UpiVerified -> fetchImage COMMON_ASSET "ny_ic_checkcircle"
                            ST.UpiNotVerified -> fetchImage COMMON_ASSET "ny_ic_info_white"
                            ST.UpiFailed -> fetchImage COMMON_ASSET "ny_ic_subtract"
                            ST.UpiVerifying -> fetchImage COMMON_ASSET "ny_ic_info_white"
                    , visibility case state.data.verificationStatus of
                        ST.UpiNotVerified -> GONE
                        ST.UpiVerifying -> GONE
                        _ -> VISIBLE
                    ]
              , progressBar
                  $ [ width $ V 16
                    , height $ V 16
                    , margin $ Margin 0 2 2 2
                    , visibility case state.data.verificationStatus of
                        ST.UpiVerifying -> VISIBLE
                        _ -> GONE
                    , progressBarColor Colors.white900
                    ]
              , textView
                  $ [ height MATCH_PARENT
                    , width WRAP_CONTENT
                    , margin $ Margin 0 0 0 2
                    , text case state.data.verificationStatus of
                        ST.UpiVerified -> getString VERIFIED
                        ST.UpiNotVerified -> getString VERIFY
                        ST.UpiFailed -> getString FAILED_STR
                        ST.UpiVerifying -> getString VERIFYING
                    , singleLine true
                    , color Colors.white900
                    ]
                  <> FontStyle.subHeading3 TypoGraphy
              ]
          ]
      , linearLayout
          [ height $ V 18
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          ]
          [ linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              ]
              [ textView
                  $ [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , text case state.data.verificationStatus of
                        ST.UpiVerified -> getString UPI_ID_VERIFIED
                        ST.UpiNotVerified -> getString ENTER_UPI_ID_IN_THE_FORMAT_NUMBERBANKNAME
                        ST.UpiFailed -> getString UPI_ID_IS_INVALID_PLEASE_CHECK_AND_REENTER
                        ST.UpiVerifying -> getString ENTER_UPI_ID_IN_THE_FORMAT_NUMBERBANKNAME
                    , color case state.data.verificationStatus of
                        ST.UpiVerified -> Colors.green900
                        ST.UpiNotVerified -> Colors.black700
                        ST.UpiFailed -> Colors.red900
                        ST.UpiVerifying -> Colors.black700
                    -- , fontStyle $ FontStyle.bold LanguageStyle
                    , gravity LEFT
                    , margin $ Margin 0 0 0 0
                    , lineHeight "28"
                    , singleLine true
                    , alpha 1.0
                    ]
                  <> FontStyle.body3 TypoGraphy
              ]
          ]
      ]

referralQrView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralQrView push state =
  let appName = fromMaybe state.data.appConfig.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  in
  PrestoAnim.animationSet
    [ PrestoAnim.Animation
        [ PrestoAnim.duration 50
        , PrestoAnim.fromAlpha 0.0
        , PrestoAnim.toAlpha 1.0
        ]
        true
    ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Colors.blackLessTrans
        , gravity CENTER
        , padding $ Padding 24 24 24 24
        ]
        [ PrestoAnim.animationSet
            [ PrestoAnim.Animation
                [ PrestoAnim.duration 100
                , PrestoAnim.fromScaleY 0.0
                , PrestoAnim.toScaleY 1.0
                , PrestoAnim.fromScaleX 0.0
                , PrestoAnim.toScaleX 1.0
                ]
                true
            ]
            $ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , gravity CENTER
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , orientation VERTICAL
                    , width MATCH_PARENT
                    , background Colors.white900
                    , padding $ Padding 16 20 16 20
                    , cornerRadius $ 12.0
                    , gravity CENTER
                    ]
                    [ textView
                        $ [ text $ getString DOWNLAOD_APP
                          , color Colors.black900
                          ]
                        <> FontStyle.h1 TypoGraphy
                    , textView
                        $ [ text $ getString SCAN_QR_CODE_TO_DOWNLOAD_THE_APP_AND_APPLY_YOUR_INVITE_CODE_AUTOMATICALLY
                          , color Colors.black800
                          , gravity CENTER
                          , margin $ MarginVertical 12 12
                          ]
                        <> FontStyle.body2 TypoGraphy
                    , imageView
                        [ qr $ Qr (generateReferralLink (getValueToLocalStore CUSTOMER_LOCATION) "share" "referral" "refer" state.data.referralCode) ((EHC.screenWidth unit) - 80) 0
                        , height $ V $ (EHC.screenWidth unit) - 150
                        , width $ V $ (EHC.screenWidth unit) - 80
                        ]
                    , linearLayout
                        [ background Colors.white900
                        , stroke $ "1," <> Colors.black500
                        , width MATCH_PARENT
                        , height WRAP_CONTENT
                        , padding $ PaddingVertical 14 14
                        , gravity CENTER
                        , cornerRadius 10.0
                        , rippleColor Colors.rippleShade
                        , onClick push $ const CloseQR
                        , margin $ MarginTop 20
                        ]
                        [ textView
                            $ [ text $ getString GO_BACK
                              , color Colors.black700
                              , padding $ PaddingBottom 3
                              ]
                            <> FontStyle.subHeading1 TypoGraphy
                        ]
                    ]
                ]
        ]

collectEarningView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
collectEarningView push state =
  let
    totalEarning = state.data.referralEarnings + state.data.referredByEarnings

    isPayoutPending = (totalEarning - state.data.referralAmountPaid) /= 0.0
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Colors.blue600
      , padding $ Padding 16 16 16 16
      , gravity CENTER
      ]
      [ textView
          $ [ text if isPayoutPending then getString YAY_YOU_HAVE_REFERRAL_EARNINGS else getString YOUR_EARNINGS_N_ <> show totalEarning 
            , color if isPayoutPending then Colors.blue900 else Colors.black800
            , gravity if isPayoutPending then CENTER else LEFT
            , singleLine false
            , padding $ PaddingBottom 3
            ]
          <> FontStyle.h2 TypoGraphy
      , linearLayout
          [ weight 1.0 ]
          []
      , if isPayoutPending then collectNowView push state else detailsView push state
      ]

collectNowView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , background Colors.blue900
    , padding $ Padding 16 8 16 8
    , cornerRadius 24.0
    , onClick push $ const AddUPI
    , rippleColor Colors.rippleShade
    , gravity CENTER
    ]
    [ textView
        $ [ text $ getString COLLECT_NOW
          , color Colors.white900
          , padding $ PaddingBottom 3
          ]
        <> FontStyle.h3 TypoGraphy
    ]

detailsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , stroke $ "1," <> Colors.blue900
    , padding $ Padding 16 8 16 8
    , cornerRadius 24.0
    , onClick push $ const ShowEarnings
    , rippleColor Colors.rippleShade
    , gravity CENTER
    ]
    [ textView
        $ [ text $ getString DETAILS
          , color Colors.blue900
          , padding $ PaddingBottom 3
          ]
        <> FontStyle.h3 TypoGraphy
    , imageView
        [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_chevron_right_blue"
        , height $ V 24
        , width $ V 24
        ]
    ]

upiAddedSuccessfully push state =
  let
    totalEarning = state.data.referralEarnings + state.data.referredByEarnings

    pendingAmount = totalEarning - state.data.referralAmountPaid
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Colors.blackLessTrans
      , gravity CENTER
      , padding $ Padding 24 24 24 24
      , onClick push $ const CloseUPI
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding $ Padding 16 0 16 16
          , background Colors.white900
          , gravity CENTER
          , cornerRadius 10.0
          ]
          [ imageView
              [ imageWithFallback "ny_ic_green_tick,"
              , height $ V 80
              , width $ V 80
              ]
          , textView
              $ [ text $ "UPI ID " <> if state.props.isUpiUpdated then getString SUBMITTED else getString UPDATED <> "!"
                , color Colors.black800
                ]
              <> FontStyle.h2 TypoGraphy
          , textView
              $ [ text $ "₹" <> show pendingAmount <> " will be credited soon to \n" <> state.data.vpa
                , text $ getString $ PENDINGAMOUNT_WILL_BE_CREDITED_SOON_TO_N_STATEDATAVPA (show pendingAmount) state.data.vpa
                , margin $ MarginTop 8
                , color Colors.black700
                , gravity CENTER
                ]
              <> FontStyle.body1 TypoGraphy
          ]
      , PrimaryButton.view (push <<< DonePrimaryButtonAC) (donePrimaryButtonConfig state)
      ]
