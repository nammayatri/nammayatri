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
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, maybe)
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
import Locale.Utils (getLanguageLocale)
import Constants (languageKey)
import Mobility.Prelude

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
        , afterRender
            ( \action -> do
                if state.props.showUPIPopUp then
                  push $ AddUPI
                else
                  pure unit
                push $ action
            )
            (const AfterRender)
        ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Colors.white900
          , padding $ PaddingVertical (EHC.safeMarginTop) (EHC.safeMarginBottom)
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
      ]
    <> (if state.props.showShareAppQr then [ referralQrView push state ] else [])
    <> (if state.props.showUpiSuccess then [ upiAddedSuccessfully push state ] else [])

shareAndEarnView push state =
  let
    totalEarning = state.data.referralEarnings + state.data.referredByEarnings
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ collectEarningView push state
      , coverView push state
      , referralDescriptionView push state
      ]

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
            , gravity CENTER
            , padding $ Padding 16 16 16 16
            ]
            [ linearLayout
                [ weight 1.0
                , height WRAP_CONTENT
                , orientation VERTICAL
                ]
                [ textView
                    $ [ text $ fromMaybe "" state.data.existingVpa
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
                [ width $ if EHC.os == "IOS" then V $ ((EHC.screenWidth unit) / 100) * 18 else WRAP_CONTENT
                , gravity RIGHT
                , height MATCH_PARENT
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , gravity CENTER
                    , padding $ Padding 12 8 12 8
                    , stroke $ "1," <> Colors.grey900
                    , cornerRadius 24.0
                    , onClick push $ const EditUPI
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
    len = DA.length state.data.history
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
                  ( \idx (PayoutItem item) ->
                      let
                        config = getPillConfig item.payoutStatus
                      in
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
                                    [ linearLayout
                                        [ width MATCH_PARENT
                                        , height WRAP_CONTENT
                                        , gravity CENTER
                                        , rippleColor Colors.rippleShade
                                        , cornerRadius 10.0
                                        , margin $ MarginBottom 4
                                        , onClick push $ const $ CopyToClipboard item.orderId
                                        ]
                                        [ linearLayout
                                            [ weight 1.0
                                            , height WRAP_CONTENT
                                            , gravity CENTER
                                            ]
                                            [ textView
                                                $ [ text $ item.orderId
                                                  , color Colors.black900
                                                  , singleLine true
                                                  , ellipsize true
                                                  ]
                                                <> FontStyle.body1 TypoGraphy
                                            ]
                                        , linearLayout
                                            [ width WRAP_CONTENT
                                            , height WRAP_CONTENT
                                            , gravity CENTER
                                            ]
                                            [ imageView
                                                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy_blue"
                                                , height $ V 16
                                                , width $ V 16
                                                ]
                                            ]
                                        ]
                                    , textView
                                        $ [ text $ (convertUTCtoISC item.payoutAt "DD MMM YYYY") <> " • " <> item.vpa
                                          , color Colors.black600
                                          , singleLine true
                                          , ellipsize true
                                          ]
                                        <> FontStyle.body1 TypoGraphy
                                    ]
                                , linearLayout
                                    [ width $ if EHC.os == "IOS" then V $ ((EHC.screenWidth unit) / 100) * 23 else WRAP_CONTENT
                                    , height WRAP_CONTENT
                                    , orientation VERTICAL
                                    , gravity RIGHT
                                    ]
                                    [ textView
                                        $ [ text $ "₹" <> (show item.amount)
                                          , color config.textColor
                                          ]
                                        <> FontStyle.subHeading2 TypoGraphy
                                    , linearLayout
                                        [ width WRAP_CONTENT
                                        , height WRAP_CONTENT
                                        , padding $ Padding 10 4 10 4
                                        , background config.color
                                        , cornerRadius 15.0
                                        ]
                                        [ textView
                                            $ [ text config.text
                                              , color Colors.white900
                                              , padding $ PaddingBottom 3
                                              ]
                                            <> FontStyle.captions TypoGraphy
                                        ]
                                    ]
                                ]
                            ]
                          <> if len == (idx + 1) then [] else [ linearLayout [ height $ V 1, background Colors.grey900, width MATCH_PARENT ] [] ]
                  )
                  state.data.history
              )
          ]
      ]

getPillConfig status = case status of
  PayoutSuccess ->
    { color: Colors.green900
    , textColor: "#14A255"
    , text: getString SUCCESS
    }
  PayoutFailed ->
    { color: Colors.red900
    , textColor: Colors.red900
    , text: "Failed"
    }
  Processing ->
    { color: Colors.yellow900
    , textColor: "#D88F00"
    , text: "Pending"
    }
  ManualReview ->
    { color: Colors.yellow900
    , textColor: "#D88F00"
    , text: "Pending"
    }

coverView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
coverView push state =
  let
    referralPayoutConfig = RemoteConfig.getReferralPayoutConfig (getValueToLocalStore CUSTOMER_LOCATION)
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , background "#B0D4EE"
      , gravity CENTER
      , orientation VERTICAL
      , padding $ Padding 20 20 20 20
      ]
      [ imageView
          [ imageWithFallback $ maybe "ny_ic_referral_r2r_cover_1," (\url -> "," <> url) (referralPayoutConfig.coverImage)
          , height $ V 254
          , width MATCH_PARENT
          ]
      , textView
          $ [ text $ getString $ INVITE_AND_EARN_ $ (show $ fromMaybe 0 $ fromNumber $ fromMaybe 0.0 referralPayoutConfig.youGet)
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
        , onClick push $ const $ CopyToClipboard state.data.referralCode
        ]
        [ textView
            $ [ text state.data.referralCode
              , color "#14171F"
              , padding $ PaddingBottom 3
              ]
            <> FontStyle.h1 TypoGraphy
        , imageView
            [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_copy_blue"
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
                  [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_qr_thumbnail_white"
                  , height $ V 28
                  , width $ V 28
                  ]
            , action: ShareQR
            }
          ]
        )

referralDescriptionView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralDescriptionView push state =
  let
    referralPayoutConfig = RemoteConfig.getReferralPayoutConfig (getValueToLocalStore CUSTOMER_LOCATION)

    youGet = fromMaybe 0 $ fromNumber $ fromMaybe 0.0 referralPayoutConfig.youGet

    theyGet = fromMaybe 0 $ fromNumber $ fromMaybe 0.0 referralPayoutConfig.theyGet
  in
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
          [ textView
              $ [ text $ getString $ YOU_GET_50__THEY_GET_50 (show youGet) (show theyGet)
                , gravity CENTER
                , color Colors.black900
                ]
              <> FontStyle.body8 TypoGraphy
          , textView
              $ [ text $ getString WHEN_YOUR_FRIEND_USES_YOUR_REFERRAL_CODE_N_AND_TAKES_THEIR_FIRST_RIDE
                , gravity CENTER
                , color Colors.black700
                ]
              <> FontStyle.body1 TypoGraphy
          , haveQuestionView push state
          ]
      ]

haveQuestionView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
haveQuestionView push state =
  let
    referralPayoutConfig = RemoteConfig.getReferralPayoutConfig (getValueToLocalStore CUSTOMER_LOCATION)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      ]
      [ textView
          $ [ text $ getString HAVE_QUESTIONS
            , color Colors.blue800
            , weight 2.0
            , onClick
                ( \action -> do
                    void $ JB.openUrlInApp referralPayoutConfig.termsLink
                    pure unit
                )
                $ const NoAction
            , rippleColor Colors.rippleShade
            , cornerRadius 24.0
            , padding $ PaddingLeft 5
            , gravity LEFT
            ]
          <> FontStyle.subHeading2 TypoGraphy
      ]

referralFaqsView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralFaqsView push state =
  let
    referralPayoutConfig = RemoteConfig.getReferralPayoutConfig (getValueToLocalStore CUSTOMER_LOCATION)
  in
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
                                            $ [ text item
                                              , color Colors.black800
                                              ]
                                            <> FontStyle.body2 TypoGraphy
                                        ]
                                  )
                                  ( [ getString TO_GET_THE_REFERRAL_REWARD_NEW_USER_SHOULD_INSTALL_THE_APP_AND_ENTER_THE_REFERRAL_CODE_BEFORE_TAKING_THEIR_FIRST_RIDE
                                    , getString THE_REFERRAL_REWARD_WILL_BE_GIVEN_ONLY_FOR_THE_FIRST_RIDE_POST_CHECKING_THE_RIDE_VALIDITY
                                    , getString RIDE_IS_CONSIDERED_VALID_BASED_ON_CERTAIN_RIDE_DISTANCE_RIDE_TIME_AND_FRAUD_CHECKS_MAINTAINED_INTERNALLY
                                    ]
                                  )
                              )
                          , linearLayout
                              [ background Colors.white900
                              , width MATCH_PARENT
                              , height WRAP_CONTENT
                              , padding $ PaddingVertical 2 2
                              , gravity CENTER
                              , cornerRadius 10.0
                              , rippleColor Colors.rippleShade
                              , onClick
                                  ( \action -> do
                                      void $ JB.openUrlInApp referralPayoutConfig.termsLink
                                      pure unit
                                  )
                                  $ const NoAction
                              ]
                              [ textView
                                  $ [ text $ getString READ_TERMS_AND_CONDITIONS
                                    , color Colors.blue900
                                    , padding $ PaddingBottom 3
                                    ]
                                  <> FontStyle.subHeading1 TypoGraphy
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

referralQrView :: forall w. (Action -> Effect Unit) -> ST.ReferralPayoutScreenState -> PrestoDOM (Effect Unit) w
referralQrView push state =
  let
    appName = fromMaybe state.data.appConfig.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
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
          , padding $ Padding 24 (EHC.safeMarginTop) 24 (EHC.safeMarginBottom)
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
                          $ [ text $ getString DOWNLOAD_APP
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

    isPayoutPending = (totalEarning - state.data.referralAmountPaid) /= 0.0 && isNothing state.data.existingVpa
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Colors.blue600
      , padding $ Padding 16 16 16 16
      , gravity CENTER
      , visibility $ boolToVisibility $ (isJust state.data.existingVpa || isPayoutPending)
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , weight 2.0
          , orientation VERTICAL
          ]
          [ textView
              $ [ text if isPayoutPending then getString YAY_YOU_HAVE_REFERRAL_EARNINGS else getString YOUR_EARNINGS_N_
                , color if isPayoutPending then Colors.blue900 else Colors.black800
                , padding $ PaddingBottom 3
                , margin $ MarginRight 10
                , height WRAP_CONTENT
                ]
              <> FontStyle.h2 TypoGraphy
          , textView
              $ [ text if isPayoutPending then "" else "₹ " <> show totalEarning
                , color if isPayoutPending then Colors.blue900 else Colors.black800
                , padding $ PaddingBottom 3
                , margin $ MarginRight 10
                , height WRAP_CONTENT
                ]
              <> FontStyle.h2 TypoGraphy
          ]
      , if isPayoutPending then collectNowView push state else detailsView push state
      ]

collectNowView push state =
  linearLayout
    ( [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , background Colors.blue900
      , padding $ Padding 16 8 16 8
      , cornerRadius 24.0
      , onClick push $ const AddUPI
      , rippleColor Colors.rippleShade
      , gravity CENTER
      ]
        <> if (getLanguageLocale languageKey) == "TA_IN" then
            [ weight 1.0 ]
          else
            [ weight 1.0 ]
    )
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
      , padding $ Padding 24 (EHC.safeMarginTop) 24 (EHC.safeMarginBottom)
      , onClick push $ const (DonePrimaryButtonAC PrimaryButton.OnClick)
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , padding $ Padding 16 16 16 16
          , background Colors.white900
          , gravity CENTER
          , cornerRadius 10.0
          ]
          [ imageView
              [ imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET "ny_ic_green_tick"
              , height $ V 80
              , width $ V 80
              ]
          , textView
              $ [ text $ "UPI ID " <> if state.props.isUpiUpdated then getString UPDATED else getString SUBMITTED <> "!"
                , color Colors.black800
                ]
              <> FontStyle.h2 TypoGraphy
          , textView
              $ [ text $ getString $ PENDINGAMOUNT_WILL_BE_CREDITED_SOON_TO_N_STATEDATAVPA ("₹" <> show pendingAmount) (fromMaybe "" state.data.existingVpa)
                , margin $ MarginTop 8
                , color Colors.black700
                , gravity CENTER
                ]
              <> FontStyle.body1 TypoGraphy
          , PrimaryButton.view (push <<< DonePrimaryButtonAC) (donePrimaryButtonConfig state)
          ]
      ]
