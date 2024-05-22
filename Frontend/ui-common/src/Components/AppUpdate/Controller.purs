module Components.AppUpdate.Controller where

import Prelude
import Font.Size as FontSize
import Font.Style (Style(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Gradient(..), Orientation(..), height, width, singleLine)
import Styles.Types (FontStyle(..), FontSize(..))
import Data.Maybe (Maybe(..))
import Common.Styles.Colors as Color
import Common.Types.App
import PrestoDOM.Animation (Animation(..))
import ConfigProvider 
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = OnClick | NoAction

type Config =
  {
     background :: String
    , titleText :: String
    , titleTextColor :: String
    , actionText :: String
    , actionTextColor :: String
    , actionTextBgColor :: String
  }

config :: Config
config = {
     background : Color.darkGreyGunmetal
    , titleText : ""
    , titleTextColor : Color.blue600
    , actionText : ""
    , actionTextColor : Color.yellow900
    , actionTextBgColor : Color.spanishYellow
}