{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DocumentCaptureScreen.ComponentConfig where 

import Components.GenericHeader as GenericHeader 
import Components.PrimaryButton as PrimaryButton 
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), Gravity(..))
import Screens.Types as ST 
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Prelude ((<>), not, (==), (>), (&&))
import Common.Types.App(LazyCheck(..))
import Font.Style as FontStyle
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Helpers.Utils as HU
import Components.ValidateDocumentModal as ValidateDocumentModal
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Resource.Constants as Constant
import Data.String as DS
import Data.Maybe (isJust, fromMaybe)
import Components.OptionsMenu as OptionsMenuConfig
import Storage (KeyStore(..), getValueToLocalStore)
import Components.BottomDrawerList as BottomDrawerList
import Data.Array as DA
import Services.API as API


primaryButtonConfig :: ST.DocumentCaptureScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
        { text = getString UPLOAD_PHOTO } 
        , margin = Margin 16 16 16 16
        , id = "DocCaptureButton"
      }
  in primaryButtonConfig'

vehicleUploadButtonConfig :: ST.DocumentCaptureScreenState -> PrimaryButton.Config
vehicleUploadButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
        { text = getString CONTINUE } 
        , margin = Margin 16 16 16 16
        , id = "vehicleUploadButton"
        , isClickable = state.props.allImagesUploaded
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.DocumentCaptureScreenState -> GenericHeader.Config
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config
    {
      height = WRAP_CONTENT
    , background = Color.transparent
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = HU.fetchImage HU.FF_ASSET "ic_new_avatar"
      , height = V 25
      , width = V 25
      , margin = Margin 12 5 5 5
      }
    , padding = PaddingVertical 5 5
    , textConfig {
        text = if DA.any (_ == getValueToLocalStore DRIVER_NAME) ["", "__failed"] then getValueToLocalStore MOBILE_NUMBER_KEY else getValueToLocalStore DRIVER_NAME
      , color = state.data.config.themeColors.onboardingHeaderTextColor
      , margin = MarginHorizontal 5 5 
      , textStyle = FontStyle.Body1
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'

appOnboardingNavBarConfig :: ST.DocumentCaptureScreenState -> AppOnboardingNavBar.Config
appOnboardingNavBarConfig state = 
  AppOnboardingNavBar.config
  { genericHeaderConfig = genericHeaderConfig state,
    headerTextConfig = AppOnboardingNavBar.config.headerTextConfig{ 
      text = Constant.transformDocText state.data.docType,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    rightButton = AppOnboardingNavBar.config.rightButton{
      text = getString HELP_FAQ,
      color = state.data.config.themeColors.onboardingHeaderTextColor
      },
    navBarOpen = state.props.menuOptions,
    prefixImageConfig = AppOnboardingNavBar.config.prefixImageConfig{ image = state.data.config.themeColors.defaultBackButton }
  }

validateDocModalState :: ST.DocumentCaptureScreenState -> ValidateDocumentModal.ValidateDocumentModalState
validateDocModalState state = 
  ValidateDocumentModal.config {
    background = Color.black,
    profilePictureCapture = false,
    verificationStatus = if state.props.validating then ST.InProgress 
                          else if isJust state.data.errorMessage then ST.Failure
                          else if not DS.null state.data.docId then ST.Success
                          else ST.None,
    verificationType = "OTHER",
    failureReason = fromMaybe "" state.data.errorMessage,
    headerConfig {
      imageConfig {
      color = Color.white900
    },
      headTextConfig {
        text = getString TAKE_PHOTO,
        color = Color.white900
      }
    }
  }

optionsMenuConfig :: ST.DocumentCaptureScreenState -> OptionsMenuConfig.Config
optionsMenuConfig state = OptionsMenuConfig.config {
  menuItems = [
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_getting_started_and_faq", textdata : "FAQs", action : "faqs", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_phone_unfilled", textdata : getString CONTACT_SUPPORT, action : "contact_support", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_language", textdata : getString CHANGE_LANGUAGE_STR, action : "change_language", isVisible : true, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_parallel_arrows_horizontal", textdata : getString CHANGE_VEHICLE, action : "change_vehicle", isVisible : state.data.config.enableChangeVehicleType, color : Color.black800},
    {image : HU.fetchImage HU.FF_ASSET "ny_ic_logout_grey", textdata : getString LOGOUT, action : "logout", isVisible :  true, color : Color.black800}
  ],
  backgroundColor = Color.blackLessTrans,
  menuBackgroundColor = Color.white900,
  gravity = RIGHT,
  menuExpanded = true,
  width = WRAP_CONTENT,
  marginRight = 16,
  itemHeight = V 50,
  itemPadding = Padding 16 16 16 16,
  cornerRadius = 4.0,
  enableAnim = true
}

bottomDrawerListConfig :: ST.DocumentCaptureScreenState -> BottomDrawerList.Config
bottomDrawerListConfig state = BottomDrawerList.config {
  animState = state.props.contactSupportModal,
  titleText = getString CONTACT_SUPPORT_VIA,
  itemList = [
    {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", visibility : true, identifier : "whatsapp"},
    {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", visibility : true, identifier : "call"}
  ]
}
