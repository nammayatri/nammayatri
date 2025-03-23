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
import JBridge(renderBase64Image)
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
import Debug (spy)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
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
        , height WRAP_CONTENT
        , scrollBarY false
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
            , padding $ Padding 16 16 16 20
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
                    , text $ getString PACKAGE_PHOTO
                    , color Color.black900
                    , gravity LEFT
                    ] <> FontStyle.subHeading3 TypoGraphy,
                    linearLayout
                            [ height WRAP_CONTENT
                            , weight 1.0
                            ][],
                    linearLayout[
                    width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , layoutGravity "right"
                    ][ PrimaryButton.view (push <<< CheckImageUploadStatus) (checkImageUploadStatusButtonConfig config)]
                ],
                linearLayout
                [
                    width MATCH_PARENT,
                    height $ V 314,
                    gravity CENTER,
                    margin $ MarginTop 8,
                    id $ EHC.getNewIDWithTag "parcelImageLayout",
                    visibility $ boolToVisibility config.imageVisibility,
                    afterRender push $ const DisplayDeliveryImageAction
                ][],
                linearLayout
                [ width $ if config.imageVisibility then WRAP_CONTENT else MATCH_PARENT
                , height $ if config.imageVisibility then WRAP_CONTENT else V 180
                , height WRAP_CONTENT
                , cornerRadius 8.0
                , margin $ Margin 0 8 0 0 
                , background Color.grey900
                , padding $ Padding 25 25 25 25
                , orientation VERTICAL
                , gravity CENTER
                , visibility $ boolToVisibility $ not config.imageVisibility
                ]
                [
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString PHOTO_NOT_TAKEN_YET
                    , color Color.black700
                    , margin (MarginBottom 10)
                    , gravity CENTER
                    ] <> FontStyle.h2 TypoGraphy,
                    textView $
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ getString PLEASE_REFRESH_ONCE_DRIVER_TAKES_PHOTO
                    , color Color.black700
                    , gravity CENTER
                    ] <> FontStyle.paragraphText LanguageStyle
                ],
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ getString PACKAGE_PHOTO_DESC
                , color Color.black700
                , margin (MarginTop 8)
                , gravity CENTER
                ] <> FontStyle.body1 TypoGraphy,
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ getString $ if config.rideStarted then END_OTP else START_OTP
                , color Color.black900
                , margin (MarginTop 24)
                , gravity LEFT
                ] <> FontStyle.subHeading1 TypoGraphy,
                textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , text $ if config.imageVisibility then config.otp else "----"
                , color Color.black900
                , gravity CENTER
                , padding (Padding 8 8 8 12)
                , margin $ MarginTop 8
                , cornerRadius 12.0
                , background Color.blue600
                ] <> FontStyle.h2 TypoGraphy,
                PrimaryButton.view (push <<< DeliveryParcelButton) primaryButtonConfig 
            ]
        ]
    ]
   ]