module Screens.DriverSavedLocationScreen.ComponentConfig where

import Data.Maybe
import Common.Types.App (LazyCheck(..))
import Components.GoToLocationModal as GoToLocationModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((<>), (==))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color

primaryButtonConfig :: ST.DriverSavedLocationScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text =
            case state.props.viewType of
              ST.GO_TO_LIST -> " Add Another Location  "
              ST.ADD_GO_TO_LOCATION -> " Select on Map "
              ST.LOCATE_ON_MAP -> "Confirm Location"
              ST.CONFIRM_LOCATION -> "Confirm Changes"
              _ -> ""
          }
        , margin = (Margin 16 15 16 24)
        , height = V 52
        , id = "Stringss"
        }
  in
    primaryButtonConfig'

locationListItemConfig :: ST.GoToLocation -> GoToLocationModal.GoToModalConfig
locationListItemConfig state =
  GoToLocationModal.config
    { id = state.id
    , lat = state.lat
    , lon = state.lon
    , address = state.address
    , tag = state.tag
    , isSelectable = false
    , isEditEnabled = true
    , isSelected = false
    , removeAcText = Just "Remove"
    , editAcText = Just "Edit"
    }

confirmDeletePopupConfig :: ST.DriverSavedLocationScreenState -> PopUpModal.Config
confirmDeletePopupConfig state =
  PopUpModal.config
    { primaryText { text = "Remove preferred location ?" }
    , secondaryText { text = "Are you sure you want to remove a preferred  location?" }
    , option1 { text = "Cancel" }
    , option2 { text = "Yes, Remove" }
    }
