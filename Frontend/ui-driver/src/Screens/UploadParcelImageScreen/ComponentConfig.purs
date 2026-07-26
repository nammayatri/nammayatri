module Screens.UploadParcelImageScreen.ComponentConfig where

import Prelude
import Data.String
import Language.Strings
import Prelude
import PrestoDOM

import Common.Types.App as Common
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Language.Types (STR(..))
import Components.GenericHeader as GenericHeader
import Screens.Types as ST
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Styles.Colors as Color
import Resource.Constants as Constant

genericHeaderConfig :: ST.UploadParcelImageScreenState -> GenericHeader.Config
genericHeaderConfig state = 
    let config = GenericHeader.config
        isBlackLayout = state.props.showConfirmAndUploadButton
        genericHeaderConfig' = config
            {
            height = WRAP_CONTENT
            , width = WRAP_CONTENT
            , background = if isBlackLayout then Color.black else Color.white900
            , prefixImageConfig {
                height = V 25
            , width = V 25
            , imageUrl = fetchImage FF_COMMON_ASSET $ if isBlackLayout then "ny_ic_chevron_left_white" else "ny_ic_chevron_left"
            , margin = Margin 8 8 8 8 
            , layoutMargin = Margin 4 4 4 4
            , enableRipple = true
            }
            , textConfig {
                text = if isBlackLayout then getString TAKE_PHOTO_OF_PARCEL else getString UPLOAD_PARCEL_IMAGE
            , color = if isBlackLayout then Color.white900 else Color.black800
            }
            , suffixImageConfig {
                visibility = GONE
            }
            , padding = (Padding 0 5 0 5)
            }
    in genericHeaderConfig'

confirmAndUploadButtonConfig :: ST.UploadParcelImageScreenState -> PrimaryButton.Config
confirmAndUploadButtonConfig state = 
    let config = PrimaryButton.config
        primaryButtonConfig' = config
            {
                textConfig{ text = getString CONFIRM_AND_UPLOAD}
            ,   width = MATCH_PARENT
            ,   height = V 48
            ,   cornerRadius = 8.0
            ,   margin = Margin 16 16 16 64
            ,   background = Color.black900
            ,   enableLoader = state.props.uploading
            ,   id = "ConfirmAndUploadButton"
            }
    in primaryButtonConfig'

primaryButtonConfig :: ST.UploadParcelImageScreenState -> PrimaryButton.Config
primaryButtonConfig state = 
    let config = PrimaryButton.config
        primaryButtonConfig' = config
            {
                textConfig{ text =  getString $ if state.props.showConfirmAndUploadButton then CONFIRM_AND_UPLOAD else UPLOAD_PHOTO }
            ,   width = MATCH_PARENT
            ,   height = V 48
            ,   cornerRadius = 8.0
            ,   margin = Margin 16 16 16 16
            ,   background = Color.black900
            ,   enableLoader = state.props.uploading
            ,   id = "UploadParcelImageButton"
            }
    in primaryButtonConfig'

uploadParcelInstructionData :: Unit -> Array {image :: String, icon :: String, instructions :: Array String}
uploadParcelInstructionData _ = 
    [
        {image: "ny_ic_parcel_correct_image", icon: "ny_ic_green_tick", instructions: [getString CLEAR_IMAGE, getString CORRECT_POSITIONING ]}
    ,   {image: "ny_ic_parcel_wrong_image", icon: "ny_ic_payment_failed", instructions: [ getString BLURRY_IMAGE, getString INCORRECT_POSITIONING ]}
    ]