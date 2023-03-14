module Screens.PermissionsScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Language.Strings
import Language.Types (STR(..))
import Prelude
import PrestoDOM
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.PermissionsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
      { text = (getString ALLOW_ACCESS)
      , color = Color.primaryButtonColor
      , textSize = FontSize.a_18}
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , background = Color.black900
      , height = (V 60)
      , alpha = if(state.props.isLocationPermissionChecked && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked) then 1.0 else 0.7
      , isClickable = (state.props.isLocationPermissionChecked && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked)
      }
  in primaryButtonConfig'