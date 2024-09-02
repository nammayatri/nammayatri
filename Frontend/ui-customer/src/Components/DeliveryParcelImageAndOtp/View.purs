module Components.DeliveryParcelImageAndOtp.View where

import Components.DeliveryParcelImageAndOtp.Controller
import Screens.Types
import Prelude
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import PrestoDOM
import PrestoDOM.Properties
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>))
import Engineering.Helpers.Commons as EHC
import PrestoDOM.Types.DomAttributes
import Effect (Effect)
import Common.Types.App (LazyCheck(..))
import JBridge as JB
import PrestoDOM.Animation as PrestoAnim
import Animation (translateYAnim,translateYAnimFromTop, fadeIn)
import Animation.Config (translateYAnimConfig)
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Data.String (length)
import Components.PrimaryButton as PrimaryButton
import Mobility.Prelude
import Data.Array (any)
import ConfigProvider

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  ]
  [ 
    PrestoAnim.animationSet [ translateYAnim translateYAnimConfig ] $
    linearLayout
    ([ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity CENTER
    , orientation VERTICAL
    , background Color.blackLessTrans
    , padding $ PaddingTop $ EHC.safeMarginTop
    , clickable true
    ] <> if EHC.os == "IOS" then [] else [adjustViewWithKeyboard "true"])
    [
        scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ]
        [
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , cornerRadius 16.0
            , background Color.white900
            , orientation VERTICAL
            , gravity CENTER_HORIZONTAL
            , margin $ Margin 24 24 24 15
            , padding $ Padding 16 16 16 8
            , alignParentBottom "true,-1"
            ][
                linearLayout 
                [
                    width MATCH_PARENT,
                    height WRAP_CONTENT,
                    orientation HORIZONTAL
                ]
                [
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text "Package Photo"
                    , color Color.black900
                    , gravity LEFT
                    ] <> FontStyle.subHeading3 TypoGraphy,
                    linearLayout[
                    width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , gravity RIGHT
                    ][ PrimaryButton.view (push <<< CheckImageUploadStatus) checkImageUploadStatusButtonConfig ]
                ],
                linearLayout
                [ width $ if config.imageVisibility then WRAP_CONTENT else V 296
                , height $ if config.imageVisibility then WRAP_CONTENT else V 180
                , height WRAP_CONTENT
                , cornerRadius 8.0
                , margin $ Margin 0 8 0 0 
                , background Color.grey900
                , gravity CENTER
                ]
                [
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text "Photo not taken yet"
                    , color Color.black700
                    , margin (MarginBottom 8)
                    , gravity CENTER
                    ] <> FontStyle.h2 TypoGraphy,
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text "Please refresh once driver takes photo of the package"
                    , color Color.black700
                    , gravity CENTER
                    ] <> FontStyle.paragraphText LanguageStyle
                ],
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text "Please ensure the package photo is correct before sharing the Start OTP with your driver.Inform driver if package photo is incorrect."
                , color Color.black700
                , gravity CENTER
                ] <> FontStyle.body1 TypoGraphy,
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text "Start OTP"
                , color Color.black900
                , gravity LEFT
                ] <> FontStyle.subHeading1 TypoGraphy,
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if config.otpVisibility then "----" else config.otp
                , color Color.black900
                , gravity CENTER
                , padding (Padding 8 8 8 12)
                , background Color.blue600
                ] <> FontStyle.h2 TypoGraphy,
                PrimaryButton.view (push <<< DeliveryParcelButton) checkImageUploadStatusButtonConfig 
            ]
        ]
    ]
   ]