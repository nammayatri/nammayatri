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


data Action = CheckImageUploadStatus PrimaryButton.Action | DeliveryParcelButton PrimaryButton.Action

type Config = {
    image :: String,
    imageVisibility :: Boolean,
    otpVisibility :: Boolean,
    otp :: String
}

config :: Config
config = {
    image: "",
    imageVisibility: false,
    otpVisibility: false,
    otp: ""
}

checkImageUploadStatusButtonConfig :: PrimaryButton.Config
checkImageUploadStatusButtonConfig = 
    PrimaryButton.config
      { textConfig
        { text = "Refresh" 
        , gravity = LEFT
        , height = WRAP_CONTENT
        , color = Color.blue800
        , textStyle = Tags
        }
      , height = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 8.0
      , padding = Padding 2 8 2 8
      , margin = MarginLeft 0
      , isPrefixImage = true
      , background = Color.blue600
      , prefixImageConfig
        { imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_refresh"
        , height = V 16
        , width = V 16
        , margin = (Margin 0 0 2 0)
        }
      , id = "checkImageUploadStatusButton"
      }

