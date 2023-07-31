module Screens.DriverSavedLocationScreen.ComponentConfig where


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
import Common.Types.App
import Data.Maybe 

primaryButtonConfig :: ST.DriverSavedLocationScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config
        { textConfig
        { text = case state.props.viewType of 
                    ST.GO_TO_LIST -> " Add Another Location  "
                    ST.ADD_GO_TO_LOCATION -> " Select on Map "
                    ST.LOCATE_ON_MAP -> "Confirm Location"
                    ST.CONFIRM_LOCATION -> "Confirm Changes"
                    _ -> ""
        , fontStyle = FontStyle.semiBold LanguageStyle
        }
        , margin = (Margin 16 15 16 24)
        , height = V 52
        , id = "Stringss"
        }
  in primaryButtonConfig'