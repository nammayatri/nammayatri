module Components.DeliveryParcelImageAndOtp.Controller where

import Prelude
import Font.Style (Style(..))
import Language.Types (STR(..))
import PrestoDOM (Padding(..), Margin(..), Gravity(..), Visibility(..), Length(..), PrestoDOM)
import Font.Size as FontSize
import Language.Strings (getString)
import Common.Types.App as Common
import Helpers.Utils as HU
import Common.Styles.Colors as Color
import Components.PrimaryButton as PrimaryButton
import Animation as Anim
import Animation.Config as AnimConfig


data Action = NoAction | DisplayDeliveryImageAction | CheckImageUploadStatus PrimaryButton.Action | DeliveryParcelButton PrimaryButton.Action

type Config = {
    image :: String,
    imageVisibility :: Boolean,
    otp :: String,
    rideStarted :: Boolean,
    refreshAnimation :: Boolean
}

config :: Config
config = {
    image: "",
    imageVisibility: false,
    otp: "",
    rideStarted : false,
    refreshAnimation : false
}

checkImageUploadStatusButtonConfig :: Config -> PrimaryButton.Config
checkImageUploadStatusButtonConfig config = 
    PrimaryButton.config
      { textConfig
        { text = getString REFRESH
        , gravity = CENTER
        , height = WRAP_CONTENT
        , color = Color.blue800
        , textStyle = Tags
        }
      , height = WRAP_CONTENT
      , width = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 8.0
      , padding = Padding 8 2 8 2
      , margin = MarginLeft 0
      , isPrefixImage = true
      , background = Color.blue600
      , prefixImageConfig
        { 
          animation = [Anim.rotateAnim (AnimConfig.rotateAnimConfig config.refreshAnimation)]
        , imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_refresh_outline"
        , height = V 10
        , width = V 10
        , margin = (Margin 0 0 2 0)
        , gravity = CENTER
        }
      , id = "checkImageUploadStatusButton"
      }

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig =
  let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
      {
        textConfig
        {
          text = getString DONE
        , color = Color.yellow900
        },
        cornerRadius = 8.0,
        background = Color.black900,
        isClickable = true,
        margin = MarginTop 20
      }
  in primaryButtonConfig'
