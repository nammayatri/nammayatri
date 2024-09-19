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
import Screens.Types as ST
import Styles.Colors as Color
import Resource.Constants as Constant

confirmAndUploadButtonConfig :: ST.UploadParcelImageScreenState -> PrimaryButton.Config
confirmAndUploadButtonConfig state = 
    let config = PrimaryButton.config
        primaryButtonConfig' = config
            {
                textConfig{ text = getString CONFIRM_AND_UPLOAD}
            ,   width = MATCH_PARENT
            ,   height = V 48
            ,   cornerRadius = 8.0
            ,   margin = Margin 16 16 16 16
            ,   background = Color.black900
            ,   enableLoader = state.props.uploading
            ,   id = "ConfirmAndUploadButton"
            }
    in primaryButtonConfig'

