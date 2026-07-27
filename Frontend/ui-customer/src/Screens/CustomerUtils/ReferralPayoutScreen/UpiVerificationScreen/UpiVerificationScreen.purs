module ScopedScreen.UpiVerificationScreen where

import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Common.Types.App hiding (FlowBT)
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
import Screens.Types as ST
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
import Debug
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (liftFlowBT)
import Engineering.Helpers.Utils (toggleLoader)
import Helpers.Utils as HU
import Prelude
import Presto.Core.Types.Language.Flow
import PrestoDOM.Core.Types.Language.Flow
import Data.Array (elem)
import Common.Types.App as Common
import Helpers.PrestoUtils
import Services.Backend as Remote
import Services.API

data UpiVerificationStatus
  = UpiVerified
  | UpiNotVerified
  | UpiFailed
  | UpiVerifying

derive instance eqUpiVerificationStatus :: Eq UpiVerificationStatus

-- Types -> The basic screen types for the screen
type State
  = { verificationStatus :: UpiVerificationStatus
    , vpa :: String
    , existingVpa :: Maybe String
    , title :: String
    , subTitle :: String
    , coverImage :: String
    , isSkipable :: Boolean
    , isEdit :: Boolean
    }

initialState :: State
initialState = { isEdit: false, isSkipable: false, verificationStatus: UpiNotVerified, vpa: "", existingVpa: Nothing, title: getString COLLECT_REFERRAL_EARNINGS, subTitle: getString VERIFY_YOUR_UPI_ID_TO_RECEIVE_YOUR_REFERRAL_EARNINGS, coverImage: fetchImage COMMON_ASSET "ny_ic_referral_r2r_cover_2" }

-- View -> The main screen call initializing initial actin and state with the screen
screen :: State -> Screen Action State ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "UpiVerificationScreen"
  , globalEvents: []
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Colors.blackLessTrans
    , gravity CENTER
    , padding $ Padding 24 24 24 24
    , onClick push $ const CloseUPI
    ]
    [ linearLayout
        [ height if EHC.os == "IOS" then MATCH_PARENT else WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const CloseUPI
        , gravity CENTER
        ]
        [ scrollView
            [ height if EHC.os == "IOS" then MATCH_PARENT else WRAP_CONTENT
            , width MATCH_PARENT
            , onClick push $ const CloseUPI
            ]
            [ linearLayout
                [ height $ if EHC.os == "IOS" then V $ ((EHC.screenHeight unit) / 100) * 90 else WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
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
                    , clipChildren true
                    ]
                    $ [ imageView
                          [ imageWithFallback $ state.coverImage
                          , height $ V $ if state.isSkipable then 144 else (EHC.screenWidth unit) - 150
                          , width $ V $ if state.isSkipable then 144 else (EHC.screenWidth unit) - 80
                          , margin $ if state.isSkipable then MarginVertical 16 16 else MarginTop 0
                          ]
                      , textView
                          $ [ text $ state.title
                            , color Colors.black800
                            , gravity CENTER
                            ]
                          <> FontStyle.h2 TypoGraphy
                      , textView
                          $ [ text $ state.subTitle
                            , margin $ MarginTop 8
                            , color Colors.black700
                            , gravity CENTER
                            ]
                          <> FontStyle.body1 TypoGraphy
                      , referralView push state
                      , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
                      ]
                    <> if state.isSkipable then [ PrimaryButton.view (push <<< MaybeLaterPrimaryButtonAC) (maybeLaterPrimaryButtonConfig state) ] else []
                ]
            ]
        ]
    ]

referralView :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
referralView push state =
  let
    isVerifyVpaEnable = DS.length state.vpa >= 6
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
                , hint $ getString ENTER_UPI_ID
                , hintColor Colors.black600
                , inputType TypeText
                , id $ EHC.getNewIDWithTag "VpaEditText"
                , accessibility ENABLE
                , accessibilityHint
                    $ case state.verificationStatus of
                        UpiVerified -> "UPI ID Verified!"
                        UpiNotVerified -> "Enter UPI ID"
                        UpiFailed -> "UPI ID verification failed"
                        UpiVerifying -> "Verifying UPI ID"
                ]
              <> FontStyle.subHeading1 LanguageStyle
              <> if state.verificationStatus == UpiVerified || isJust state.existingVpa then [ text $ fromMaybe state.vpa state.existingVpa, setCursorAtEnd true ] else []
          , linearLayout
              [ height MATCH_PARENT
              , width WRAP_CONTENT
              , cornerRadius 20.0
              , gravity CENTER_VERTICAL
              , margin $ Margin 10 10 10 10
              , padding $ Padding 12 4 12 4
              , onClick push $ const VerifyVPA
              , clickable $ isVerifyVpaEnable && state.verificationStatus == UpiNotVerified
              , alpha if isVerifyVpaEnable then 1.0 else 0.4
              , background case state.verificationStatus of
                  UpiVerified -> Colors.green900
                  UpiNotVerified -> Colors.blue800
                  UpiFailed -> Colors.red900
                  UpiVerifying -> Colors.black700
              ]
              [ imageView
                  $ [ width
                        $ V
                            ( case state.verificationStatus of
                                UpiNotVerified -> 0
                                UpiVerifying -> 0
                                _ -> 18
                            )
                    , height
                        $ V
                            ( case state.verificationStatus of
                                UpiNotVerified -> 0
                                UpiVerifying -> 0
                                _ -> 18
                            )
                    , margin $ Margin 0 2 2 2
                    , imageWithFallback
                        $ case state.verificationStatus of
                            UpiVerified -> fetchImage COMMON_ASSET "ny_ic_checkcircle"
                            UpiNotVerified -> fetchImage COMMON_ASSET "ny_ic_info_white"
                            UpiFailed -> fetchImage COMMON_ASSET "ny_ic_subtract"
                            UpiVerifying -> fetchImage COMMON_ASSET "ny_ic_info_white"
                    , visibility case state.verificationStatus of
                        UpiNotVerified -> GONE
                        UpiVerifying -> GONE
                        _ -> VISIBLE
                    ]
              , progressBar
                  $ [ width $ V 16
                    , height $ V 16
                    , margin $ Margin 0 2 2 2
                    , visibility case state.verificationStatus of
                        UpiVerifying -> VISIBLE
                        _ -> GONE
                    , progressBarColor Colors.white900
                    ]
              , textView
                  $ [ height MATCH_PARENT
                    , width WRAP_CONTENT
                    , margin $ Margin 0 0 0 2
                    , text case state.verificationStatus of
                        UpiVerified -> getString VERIFIED
                        UpiNotVerified -> getString VERIFY
                        UpiFailed -> getString FAILED_STR
                        UpiVerifying -> getString VERIFYING
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
                    , text case state.verificationStatus of
                        UpiVerified -> getString UPI_ID_VERIFIED
                        UpiNotVerified -> getString ENTER_UPI_ID_IN_THE_FORMAT_NUMBERBANKNAME
                        UpiFailed -> getString UPI_ID_IS_INVALID_PLEASE_CHECK_AND_REENTER
                        UpiVerifying -> getString ENTER_UPI_ID_IN_THE_FORMAT_NUMBERBANKNAME
                    , color case state.verificationStatus of
                        UpiVerified -> Colors.green900
                        UpiNotVerified -> Colors.black700
                        UpiFailed -> Colors.red900
                        UpiVerifying -> Colors.black700
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

primaryButtonConfig :: State -> PrimaryButton.Config
primaryButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString if state.isEdit then UPDATE_UPI_ID else COLLECT_EARNINGS_NOW
      }
    , margin = MarginTop 24
    , alpha = if state.verificationStatus == UpiVerified then 1.0 else 0.4
    , isClickable = state.verificationStatus == UpiVerified
    , id = "ReferralPayoutScreenPB"
    }

maybeLaterPrimaryButtonConfig :: State -> PrimaryButton.Config
maybeLaterPrimaryButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString MAYBE_LATER
      , color = Colors.black700
      }
    , margin = MarginTop 16
    , id = "ReferralPayoutScreenMaybeLaterPB"
    , background = Colors.white900
    , enableRipple = true
    , stroke = "0," <> Colors.white900
    , rippleColor = Colors.rippleShade
    }

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = NextScreen State
  | Close State
  | VerifyVPAOut State
  | AddVpa State

data Action
  = VerifyVPA
  | VpaTextChanged String
  | CloseUPI
  | PrimaryButtonActionController PrimaryButton.Action
  | MaybeLaterPrimaryButtonAC PrimaryButton.Action

instance showAction :: Show Action where
  show VerifyVPA = "VerifyVPA"
  show (VpaTextChanged _) = "VpaTextChanged"
  show CloseUPI = "CloseUPI"
  show (PrimaryButtonActionController act) = "AddUPI_" <> show act
  show (MaybeLaterPrimaryButtonAC act) = "MaybeLaterPrimaryButtonAC_" <> show act

instance loggableAction :: Loggable Action where
  performLog action appId = pure unit

eval :: Action -> State -> Eval Action ScreenOutput State
eval CloseUPI state = exit $ Close state

eval (VpaTextChanged upi) state = continue state { vpa = upi, verificationStatus = UpiNotVerified }

eval VerifyVPA state = updateAndExit state { verificationStatus = UpiVerifying } $ VerifyVPAOut state

eval (PrimaryButtonActionController act) state = case act of
  PrimaryButton.OnClick -> exit $ AddVpa state
  _ -> continue state

eval (MaybeLaterPrimaryButtonAC act) state = case act of
  PrimaryButton.OnClick -> exit $ Close state
  _ -> continue state

eval _ state = continue state

upiNotVerificationScreen :: State -> FlowBT String (Maybe String)
upiNotVerificationScreen state = do
  act <- lift $ lift $ showScreen $ screen state
  case act of
    VerifyVPAOut updatedState -> do
      resp <- lift $ lift $ Remote.verifyVpa updatedState.vpa
      case resp of
        Right (VerifyVPAResp respData) -> do
          upiNotVerificationScreen $ updatedState { verificationStatus = if fromMaybe false respData.isValid then UpiVerified else UpiFailed, vpa = fromMaybe state.vpa respData.vpa }
        Left resp -> do
          upiNotVerificationScreen $ updatedState { verificationStatus = UpiFailed }
    Close _ -> pure Nothing
    AddVpa updatedState -> do
      resp <- lift $ lift $ Remote.updateVpa updatedState.vpa
      let
        _ = JB.hideKeyboardOnNavigation true
      case resp of
        Right _ -> do
          void $ lift $ lift do
            resp <- Remote.getProfile ""
            case resp of
              Right respData -> void $ modifyState $ \(GlobalState state) -> GlobalState $ state { globalFlowCache { profileResp = Just respData } }
              Left _ -> pure unit
            pure unit
          let _ = EHC.setText (EHC.getNewIDWithTag "VpaEditText") ""
          pure $ Just updatedState.vpa
        -- modifyScreenState $ ReferralPayoutScreenStateType (\referralPayoutScreen -> referralPayoutScreen{props{showUpiSuccess = true, showUPIPopUp = false}, data{ existingVpa = Just updatedState.vpa}})
        Left resp -> upiNotVerificationScreen $ updatedState { verificationStatus = UpiFailed }
    _ -> upiNotVerificationScreen state
